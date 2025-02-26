(ns artist-connections.server
  (:require [integrant.core :as ig]
            [ring.adapter.jetty :as jetty]))

(defmethod ig/init-key :artist-connections/server [_ {:keys [port handler]}]
  (println "Starting server on port" port)
  (jetty/run-jetty handler {:port port :join? false :send-server-version? false}))

(defmethod ig/halt-key! :artist-connections/server [_ server]
  (.stop server))