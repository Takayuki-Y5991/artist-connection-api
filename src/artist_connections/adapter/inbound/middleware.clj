(ns artist-connections.adapter.inbound.middleware
  (:require
   [artist-connections.adapter.inbound.response :as inbound.response]
   [artist-connections.macros.railway :as r]
   [cheshire.core :as json]
   [clojure.core.async :as async]
   [ring.util.response :as response]))

(defn- process-railway-result
  "Railway結果をRingレスポンスに変換します。
   プロトコルベースの新しいRailway実装に対応しています。"
  [result]
  (println "Processing railway result of type:" (type result))

  (cond
    ;; RailwayResultを実装している場合
    (r/railway-result? result)
    (if (r/success? result)
      (let [value (r/unwrap result)]
        (println "PROCESSING Success VALUE. VALUE TYPE:" (type value))
        (cond
          ;; ResponseFormatterプロトコルを実装している場合
          (and (some? value) (satisfies? inbound.response/ResponseFormatter value))
          (do
            (println "PROCESSING ResponseFormatter")
            (inbound.response/->response value))

          ;; すでにRingレスポンスの場合
          (and (map? value) (contains? value :status))
          (do
            (println "ALREADY A RING RESPONSE")
            value)

          ;; BodyContainerの場合（SuccessResponseと似た構造）
          (and (map? value) (:body value))
          (do
            (println "PROCESSING Body Container")
            (-> (:body value)
                (response/response)
                (response/status 200)
                (response/content-type "application/json")))

          ;; その他の場合
          :else
          (do
            (println "PROCESSING Generic Success Value")
            (-> value
                (response/response)
                (response/status 200)
                (response/content-type "application/json")))))

      ;; 失敗の場合
      (let [error (r/unwrap result)]
        (println "PROCESSING Failure ERROR. ERROR TYPE:" (type error))
        (cond
          ;; ResponseFormatterプロトコルを実装している場合
          (and (some? error) (satisfies? inbound.response/ResponseFormatter error))
          (do
            (println "PROCESSING ResponseFormatter ERROR")
            (inbound.response/->response error))

          ;; エラーレスポンス形式の場合
          (and (map? error) (:status error) (:message error))
          (do
            (println "PROCESSING Structured Error")
            (-> {:error_code (name (:status error))
                 :message (:message error)}
                (response/response)
                (response/status (:status error))
                (response/content-type "application/json")))

          ;; ex-infoの例外の場合
          (instance? clojure.lang.ExceptionInfo error)
          (let [data (ex-data error)]
            (println "PROCESSING ExceptionInfo:" data)
            (-> {:error_code (or (:error_code data) "error")
                 :message (or (:message data) (.getMessage error))}
                (response/response)
                (response/status (or (:status data) 500))
                (response/content-type "application/json")))

          ;; 他の例外の場合
          (instance? Throwable error)
          (do
            (println "PROCESSING Exception:" (.getMessage error))
            (-> {:error_code "internal_error"
                 :message (.getMessage error)}
                (response/response)
                (response/status 500)
                (response/content-type "application/json")))

          ;; その他のエラーの場合
          :else
          (do
            (println "PROCESSING Generic Error")
            (-> {:error (str error)}
                (response/response)
                (response/status 500)
                (response/content-type "application/json"))))))

    ;; すでにRingレスポンスの場合
    (and (map? result) (contains? result :status))
    (do
      (println "ALREADY A RING RESPONSE (Direct)")
      result)

    ;; futureの場合
    (future? result)
    (do
      (println "PROCESSING Future")
      (process-railway-result @result))

    ;; core.asyncのチャネルの場合
    (instance? clojure.core.async.impl.channels.ManyToManyChannel result)
    (do
      (println "PROCESSING core.async Channel")
      (let [val (async/<!! result)]
        (println "Channel returned value of type:" (type val))
        (process-railway-result val)))

    ;; その他の場合
    :else
    (do
      (println "FALLBACK CASE - Converting to Response. TYPE:" (type result))
      (-> result
          (response/response)
          (response/status 200)
          (response/content-type "application/json")))))

(defn wrap-railway-response
  "Railway結果をRingレスポンスに変換するミドルウェア。
   新しいRailway実装（RailwayResultプロトコル）に対応しています。"
  [handler]
  (fn [request]
    (println "ENTERING wrap-railway-response middleware")
    (let [result (handler request)]
      (println "Handler returned result of type:" (type result))
      (let [response (process-railway-result result)]
        (println "EXITING wrap-railway-response with response type:" (type response))
        response))))

(defn wrap-error-handling
  "例外をキャッチしてRailway失敗結果に変換するミドルウェア"
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable e
        (println "ERROR HANDLING MIDDLEWARE caught exception:" (.getMessage e))
        (process-railway-result (r/failure e))))))

(defn wrap-json-response
  "レスポンスをJSON形式に変換するミドルウェア"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (println "IN wrap-json-response - RESPONSE:" response)
      (cond
        ;; 既に Content-Type が application/json の場合
        (= (get-in response [:headers "Content-Type"]) "application/json")
        (do
          (println "ALREADY JSON CONTENT TYPE")
          ;; ボディが文字列でない場合は JSON に変換
          (if (and (:body response) (not (string? (:body response))))
            (do
              (println "CONVERTING BODY TO JSON STRING")
              (update response :body json/generate-string))
            response))

        ;; その他の場合
        :else
        (do
          (println "SETTING JSON CONTENT TYPE AND CONVERTING BODY")
          (-> response
              (update :body #(if (string? %) % (json/generate-string %)))
              (response/content-type "application/json")))))))

(defn debug-middleware
  "デバッグ情報をログに出力するミドルウェア"
  [handler]
  (fn [request]
    (println "DEBUG MIDDLEWARE - REQUEST:" request)
    (let [result (handler request)]
      (println "DEBUG MIDDLEWARE - RESULT:" result)
      (if (r/railway-result? result)
        (println "DEBUG MIDDLEWARE - RAILWAY SUCCESS?:" (r/success? result))
        (println "DEBUG MIDDLEWARE - NOT RAILWAY RESULT"))
      result)))

;; 非同期処理に特化したミドルウェア
(defn wrap-async-railway
  "非同期Railway結果をRingレスポンスに変換するミドルウェア。
   チャネルやFutureを扱いながら、非同期的に動作します。"
  [handler]
  (fn [request]
    (println "ENTERING wrap-async-railway middleware")
    (let [result (handler request)]
      (cond
        ;; Future の場合
        (future? result)
        (do
          (println "HANDLING FUTURE RESULT")
          (let [response-future (future (process-railway-result @result))]
            @response-future))

        ;; core.async チャネルの場合
        (instance? clojure.core.async.impl.channels.ManyToManyChannel result)
        (do
          (println "HANDLING CHANNEL RESULT")
          (process-railway-result (async/<!! result)))

        ;; 標準的な結果の場合
        :else
        (do
          (println "HANDLING SYNCHRONOUS RESULT")
          (process-railway-result result))))))

;; Reitit用middleware関数
(defn railway-middleware
  "Railway結果に対応したReitit用middleware関数"
  []
  {:name ::railway-middleware
   :wrap wrap-railway-response})

(defn error-middleware
  "エラー処理に対応したReitit用middleware関数"
  []
  {:name ::error-middleware
   :wrap wrap-error-handling})

(defn json-middleware
  "JSON処理に対応したReitit用middleware関数"
  []
  {:name ::json-middleware
   :wrap wrap-json-response})

(defn debug-middleware-fn
  "デバッグに対応したReitit用middleware関数"
  []
  {:name ::debug-middleware
   :wrap debug-middleware})

(defn async-railway-middleware
  "非同期Railway処理に対応したReitit用middleware関数"
  []
  {:name ::async-railway-middleware
   :wrap wrap-async-railway})