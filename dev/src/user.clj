(ns user
  (:require
   [artist-connections.config :as config]
   [clojure.java.io :as io]
   [clojure.tools.namespace.repl :as namespace.repl]
   [integrant.repl :as repl]))

(def go repl/go)
(def halt repl/halt)
(def reset repl/reset)
(def clear repl/clear)

(namespace.repl/set-refresh-dirs "dev/src" "src" "test")

;; (repl/set-prep!
;;  #(config/load-config :dev))
(repl/set-prep!
 #(config/read-config (io/resource "config.edn")))

(defn reset-all []
  (repl/reset)
  (namespace.repl/refresh))