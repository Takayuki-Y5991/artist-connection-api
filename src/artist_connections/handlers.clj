(ns artist-connections.handlers
  (:require
   [artist-connections.adapter.inbound.middleware :as middleware]
   [integrant.core :as ig]
   [reitit.ring :as ring]))

(defmethod ig/init-key :artist-connections/handlers [_ {:keys [router]}]
  (-> (ring/ring-handler
       router
       (ring/create-default-handler))
      ;; 明示的にミドルウェアをグローバルに適用
      (middleware/wrap-railway-response)
      (middleware/wrap-json-response)
      (middleware/wrap-error-handling)
      (middleware/debug-middleware)))