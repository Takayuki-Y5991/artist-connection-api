(ns artist-connections.adapter.inbound.response
  (:require
   [artist-connections.macros.railway :refer [delay-success failure
                                              lazy-failure lazy-success
                                              success]]
   [clojure.core.async :refer [<! go]]
   [ring.util.response :as response]))

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

;; Synchronous success response
(defn ok [body]
  (success (->SuccessResponse body)))

;; Synchronous error response
(defn error [status message]
  (failure (->ErrorResponse status message)))

;; Lazy success response (for expensive computations)
(defn lazy-ok [body-fn]
  (lazy-success #(->SuccessResponse (body-fn))))

;; Lazy error response
(defn lazy-error [status-fn message-fn]
  (lazy-failure #(->ErrorResponse (status-fn) (message-fn))))

;; Convenience function for delayed expensive computation
(defn delay-ok [body-expr]
  (delay-success (->SuccessResponse body-expr)))

;; Asynchronous success response
(defn async-ok [body-ch]
  (go
    (success (->SuccessResponse (<! body-ch)))))

;; Asynchronous error response
(defn async-error [status message-ch]
  (go
    (failure (->ErrorResponse status (<! message-ch)))))

;; Async/Lazy combined - Async computation that returns a lazy result
;; Useful for computations that might not be needed immediately
(defn async-lazy-ok [body-fn-ch]
  (go
    (lazy-success (<! body-fn-ch))))