(ns artist-connections.adapter.inbound.response
  (:require [ring.util.response :as response]
            [artist-connections.macros.railway :as r]))

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

;; 同期バージョン
(defn ok [body]
  (r/success (->SuccessResponse body)))

(defn error [status message]
  (r/failure (->ErrorResponse status message)))

;; 遅延評価バージョン
(defn ok-defer [body-fn]
  (r/success-defer #(->SuccessResponse (body-fn))))

(defn error-defer [status message-fn]
  (r/failure-defer #(->ErrorResponse status (message-fn))))

;; 非同期バージョン (Future)
(defn ok-future [body-fn]
  (r/success-future #(->SuccessResponse (body-fn))))

(defn error-future [status message-fn]
  (r/failure-future #(->ErrorResponse status (message-fn))))

;; 非同期バージョン (core.async)
(defn ok-go [body-fn]
  (r/success-go #(->SuccessResponse (body-fn))))

(defn error-go [status message-fn]
  (r/failure-go #(->ErrorResponse status (message-fn))))