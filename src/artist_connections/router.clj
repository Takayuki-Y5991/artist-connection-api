(ns artist-connections.router
  (:require
   [artist-connections.adapter.inbound.middleware :as middleware]
   [integrant.core :as ig]
   [muuntaja.core :as m]
   [reitit.ring :as ring]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [artist-connections.adapter.inbound.handlers.health]))

(defn- process-handler [handler-config]
  (if (keyword? (:handler handler-config))
    (assoc handler-config :handler (ig/init-key (:handler handler-config) {}))
    handler-config))

(defn compile-middleware [middleware]
  (fn [handler]
    (reduce
     (fn [handler middleware]
       (middleware handler))
     handler
     (reverse middleware))))

(defmethod ig/init-key :artist-connections/router [_ {:keys [routes]}]
  (ring/router
   (for [[path methods] routes]
     [(str "/" path)
      (reduce-kv
       (fn [acc method config]
         (assoc acc method (process-handler config)))
       {}
       methods)])
   {:data {:muuntaja m/instance
           :middleware [muuntaja/format-middleware]}}))
