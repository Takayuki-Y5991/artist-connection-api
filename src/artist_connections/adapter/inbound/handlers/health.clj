(ns artist-connections.adapter.inbound.handlers.health
  "Health check handler implementation"
  {:added "1.0"}
  (:require [integrant.core :as ig]
            [artist-connections.adapter.inbound.response :refer [ok]]
            [artist-connections.port.inbound.handlers.health :refer [HealthPort health-check]]))

(defn health-check-handler [_]
  (ok
   {:status "ok"
    :timestamp (str (java.time.Instant/now))}))


(defrecord HealthHandler []
  HealthPort
  (health-check [_ request]
    (health-check-handler request)))

(defn health-handler [request]
  (let [handler (->HealthHandler)]
    (health-check handler request)))

(defmethod ig/init-key :artist-connections.adapter.inbound.handlers.health/check [_ _]
  health-handler)
