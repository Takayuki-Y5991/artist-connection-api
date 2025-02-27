(ns artist-connections.adapter.inbound.middleware
  (:require
   [artist-connections.macros.railway :refer [failure? success?]]
   [cheshire.core :as json]
   [ring.util.response :as response]))

(defn wrap-json-response [handler]
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

(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (-> (response/response {:error_code "internal_error"
                                :message (.getMessage e)})
            (response/status 500)
            (response/content-type "application/json"))))))

(defn wrap-railway-response [handler]
  (fn [request]
    (println "ENTERING wrap-railway-response middleware")
    (let [result (handler request)]
      (println "IN wrap-railway-response - RESULT TYPE:" (type result))

      (let [response (cond
                       (success? result)
                       (let [value (:value result)]
                         (println "PROCESSING Success TYPE. VALUE TYPE:" (type value))
                         (cond
                           ;; SuccessResponse の場合
                          ;;  (instance? artist_connections.adapter.inbound.response.SuccessResponse value)
                           (and (map? value) (:body value))
                           (do
                             (println "PROCESSING SuccessResponse")
                             (-> (:body value)
                                 (response/response)
                                 (response/status 200)
                                 (response/content-type "application/json")))

                           ;; その他の場合
                           :else
                           (do
                             (println "PROCESSING Other Value Type")
                             (-> value
                                 (response/response)
                                 (response/status 200)
                                 (response/content-type "application/json")))))

                       (failure? result)
                       (let [error (:error result)]
                         (println "PROCESSING Failure Type. ERROR TYPE:" (type error))
                         (cond
                           (and (map? error) (:status error) (:message error))
                           (do
                             (println "PROCESSING ErrorResponse")
                             (-> {:error_code (name (:status error))
                                  :message (:message error)}
                                 (response/response)
                                 (response/status (:status error))
                                 (response/content-type "application/json")))

                           ;; その他の場合
                           :else
                           (do
                             (println "PROCESSING Other Error Type")
                             (-> {:error error}
                                 (response/response)
                                 (response/status 500)
                                 (response/content-type "application/json")))))

                       ;; 既に Ring レスポンスの場合
                       (and (map? result) (contains? result :status))
                       (do
                         (println "ALREADY A RING RESPONSE")
                         result)

                       ;; その他の場合
                       :else
                       (do
                         (println "FALLBACK CASE - TYPE:" (type result))
                         (-> result
                             (response/response)
                             (response/status 200)
                             (response/content-type "application/json"))))]

        (println "EXITING wrap-railway-response - RESPONSE TYPE:" (type response))
        response))))



(defn debug-response-middleware [handler]
  (fn [request]
    (println "BEFORE HANDLER - REQUEST:" request)
    (let [response (handler request)]
      (println "AFTER HANDLER - RESPONSE TYPE:" (type response))
      (println "AFTER HANDLER - RESPONSE:" response)

      (if (and (map? response)
               (:body response)
               (map? (:body response))
               (:body (:body response)))
        (update response :body :body)
        response))))