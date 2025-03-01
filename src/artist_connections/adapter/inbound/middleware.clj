(ns artist-connections.adapter.inbound.middleware
  (:require
   [artist-connections.macros.railway :refer [failure? success? lazy? ->value ->error]]
   [artist-connections.macros.railway-async :as a :refer [channel?]]
   [cheshire.core :as json]
   [clojure.core.async :refer [<! go <!]]
   [ring.util.response :as response]))

;; Helper to synchronously extract values from channel
(defn <!! [ch]
  (let [p (promise)]
    (go
      (let [v (<! ch)]
        (deliver p v)))
    (deref p 5000 {:status 500
                   :body {:error "Timeout while processing request"}
                   :headers {"Content-Type" "application/json"}})))

;; JSON response middleware (unchanged)
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

;; Error handling middleware (unchanged)
(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (-> (response/response {:error_code "internal_error"
                                :message (.getMessage e)})
            (response/status 500)
            (response/content-type "application/json"))))))

;; Process railway result into HTTP response
(defn process-railway-result [result]
  (println "PROCESSING RAILWAY RESULT - TYPE:" (type result))

  (cond
    ;; Success case
    (success? result)
    (let [value (if (lazy? result) (->value result) (:value result))]
      (println "PROCESSING Success TYPE. VALUE TYPE:" (type value))
      (cond
        ;; SuccessResponse case
        (and (map? value) (:body value))
        (do
          (println "PROCESSING SuccessResponse")
          (-> (:body value)
              (response/response)
              (response/status 200)
              (response/content-type "application/json")))

        ;; Other cases
        :else
        (do
          (println "PROCESSING Other Value Type")
          (-> value
              (response/response)
              (response/status 200)
              (response/content-type "application/json")))))

    ;; Failure case
    (failure? result)
    (let [error (if (lazy? result) (->error result) (:error result))]
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

        ;; Other cases
        :else
        (do
          (println "PROCESSING Other Error Type")
          (-> {:error error}
              (response/response)
              (response/status 500)
              (response/content-type "application/json")))))

    ;; Already a Ring response
    (and (map? result) (contains? result :status))
    (do
      (println "ALREADY A RING RESPONSE")
      result)

    ;; Fallback case
    :else
    (do
      (println "FALLBACK CASE - TYPE:" (type result))
      (-> result
          (response/response)
          (response/status 200)
          (response/content-type "application/json")))))

;; Enhanced middleware to handle sync, async, and lazy railway results
(defn wrap-railway-response [handler]
  (fn [request]
    (println "ENTERING wrap-railway-response middleware")
    (let [result (handler request)]
      (println "IN wrap-railway-response - RESULT TYPE:" (type result))

      (cond
        ;; Handle async (channel) results
        (channel? result)
        (do
          (println "HANDLING ASYNC RESULT")
          (<!! (go (process-railway-result (<! result)))))

        ;; Handle lazy results
        (lazy? result)
        (do
          (println "HANDLING LAZY RESULT")
          (process-railway-result result))

        ;; Handle synchronous results
        :else
        (process-railway-result result)))))

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