(ns artist-connections.adapter.inbound.middleware
  (:require [ring.util.response :as response]
            [cheshire.core :as json]))

(defn wrap-json-response [handler]
  (fn [request]
    (let [response (handler request)]
      (if (= get-in response [:headers "Content-Type"] "application/json")
        response
        (-> response
            (update :body #(if (string? %) % (json/generate-string %)))
            (response/content-type "application/json"))))))

(defn wrap-error-handling [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        (response/response {:error_code "internal_error"
                            :message (.getMessage e)})
        (response/status 500)))))