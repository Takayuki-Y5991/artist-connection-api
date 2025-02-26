(ns artist-connections.adapter.inbound.response
  (:require [ring.util.response :as response]
            [artist-connections.macros.railway :refer [success failure]]))

(defprotocol ResponseFormatter
  (->response [this]))

(defrecord SuccessResponse [body]
  ResponseFormatter
  (->response [_]
    (-> body
        (response/response)
        (response/status 200)
        (response/content-type "application/json"))))

(defrecord ErrorResponse [status message]
  ResponseFormatter
  (->response [_]
    (-> {:error_code (name status)
         :message message}
        (response/response)
        (response/status status)
        (response/content-type "application/json"))))

(defn map->response [m]
  (if (contains? m :status)
    m  ; 既にRingレスポンス
    (-> m
        (response/response)
        (response/status 200)
        (response/content-type "application/json"))))


(defn ok [body]
  (success (->SuccessResponse body)))

(defn error [status message]
  (failure (->ErrorResponse status message)))
