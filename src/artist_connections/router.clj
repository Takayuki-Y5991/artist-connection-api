(ns artist-connections.router
  (:require
   [artist-connections.adapter.inbound.middleware :as middleware]
   [integrant.core :as ig]
   [muuntaja.core :as m]
   [reitit.ring :as ring]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [artist-connections.adapter.inbound.handlers.health]))

(defn- resolve-handler [handler-key]
  (when (keyword? handler-key)
    (println "Resolving handler:" handler-key)  ; デバッグ用
    (ig/init-key handler-key {})))  ; Integrantを使用してハンドラーを初期化

(defn- resolve-middleware [middleware-key]
  (when (keyword? middleware-key)
    (resolve (symbol (namespace middleware-key)
                     (name middleware-key)))))

(defn- process-route-config [route-config]
  (println "Processing route config:" route-config)
  (cond-> route-config
    (:handler route-config) (update :handler #(if (keyword? %)
                                                (ig/init-key % {})
                                                %))))

(defmethod ig/init-key :artist-connections/router [_ {:keys [routes]}]
  (println "Initializing router with routes:" routes)
  (ring/router
   (vec
    (for [[path methods] routes]
      [(str "/" (name path)) (reduce-kv
                              (fn [acc method handler-config]
                                (assoc acc method (process-route-config handler-config)))
                              {}
                              methods)]))
   {:data {:muuntaja m/instance
           :middleware [muuntaja/format-middleware]}}))

