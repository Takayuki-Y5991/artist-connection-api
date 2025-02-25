(ns artist-connections.handlers
  (:require [integrant.core :as ig]
            [reitit.ring :as ring]))

(defmethod ig/init-key :artist-connections/handlers [_ {:keys [router]}]
  (ring/ring-handler
   router
   (ring/create-default-handler)))
