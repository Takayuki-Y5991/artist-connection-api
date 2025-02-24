(ns artist-connections.system
  (:require [integrant.core :as ig]
            [artist-connections.config :as config]))

(defn start [profile]
  (let [config (config/read-config "resources/config.edn" {:profile profile})]
    (println "Starting system with config:" config)
    (try
      (ig/init config)
      (catch Exception e
        (println "Failed to start system:" (.getMessage e))
        (throw e)))))

(defn -main [& _]
  (start :prod))