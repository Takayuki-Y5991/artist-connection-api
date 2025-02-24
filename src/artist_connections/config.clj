(ns artist-connections.config
  (:require [aero.core :as aero]
            [integrant.core :as ig]
            [artist-connections.adapter.inbound.handlers.health]))

(defmethod aero/reader 'ig/ref
  [_ _ value]
  (ig/ref value))

(defn- env [val]
  (System/getenv (name val)))

(defn- merge-default-readers [readers]
  (merge
   {'env env
    'ig/ref ig/ref}
   readers))

(defn read-config
  ([source]
   (read-config source {}))
  ([source {:keys [profile] :or {profile :dev}}]
   (let [config (aero/read-config source {:profile profile})]
     (println "Read config:" config)
     (ig/load-namespaces config)
     config)))
