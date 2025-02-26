(ns artist-connections.adapter.outbound.client.auth-client-adapter
  "Adapter for the auth client port."
  {:added "1.0"}
  (:require
   [artist-connections.adapter.outbound.client.circuit-breaker :refer [create-circuit-breaker]]
   [artist-connections.adapter.outbound.client.retry :refer [retry
                                                             retryable-error?]]
   [artist-connections.macros.railway :refer [!>]]
   [artist-connections.port.outbound.client.auth-client :refer [AuthClientPort]]
   [cheshire.core :as json]
   [clojure.tools.logging :as log]
   [integrant.core :as ig]
   [org.httpkit.client :as http]))

(defrecord HttpAuthClient [base-uri circuit-breaker]
  AuthClientPort

  (request-redirect-uri [_ params]
    ((circuit-breaker
      (retry
       (fn [params]
         (!> (let [request-body params
                   _ (log/debug "request-redirect-uri:" {:url (str base-uri "/auth/redirect")})
                   response @(http/post (str base-uri "/auth/redirect")
                                        {:body (json/generate-string request-body)
                                         :headers {"Content-Type" "application/json"}
                                         :as :text})]
               (if (= 200 (:status response))
                 (do
                   (log/debug "Redirect URI request successful")
                   (json/parse-string (:body response) true))
                 (do
                   (log/warn "Redirect URI request failed with status" (:status response))
                   (throw (ex-info "Failed to get redirect URI"
                                   {:status (:status response)
                                    :body (:body response)})))))))
       {:max-attempts 3
        :backoff-ms 500
        :retryable? retryable-error?}))
     params))

  (callback [_ params]
    ((circuit-breaker
      (retry
       (fn [params]
         (!>
          (let [request-params params
                _ (log/debug "callback:" {:url (str base-uri "/auth/callback")})
                response @(http/get (str base-uri "/auth/callback")
                                    {:query-params request-params
                                     :as :text})]
            (if (= 200 (:status response))
              (do
                (log/debug "Callback request successful")
                (json/parse-string (:body response) true))
              (do
                (log/warn "Callback request failed with status" (:status response))
                (throw (ex-info "Failed to get callback"
                                {:status (:status response)
                                 :body (:body response)})))))))
       {:max-attempts 3
        :backoff-ms 500
        :retryable? retryable-error?}))
     params)))


(defmethod ig/init-key :artist-connections.adapter.outbound.client.auth-client/auth-client [_ {:keys [base-uri circuit-breaker-opts]}]
  (let [circuit-breaker (create-circuit-breaker
                         (or circuit-breaker-opts {:failure-threshold 5
                                                   :reset-timeout-ms 30000}))]
    (->HttpAuthClient base-uri circuit-breaker)))