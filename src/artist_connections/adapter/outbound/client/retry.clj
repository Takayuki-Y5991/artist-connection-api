(ns artist-connections.adapter.outbound.client.retry
  (:require
   [artist-connections.macros.railway :refer [success?]]
   [clojure.core.async :refer [<!! timeout]]))

(defn retryable-error? [error]
  (let [status (get-in error [:details :status])]
    (or (nil? status)
        (#{408 429 500 502 503 504} status))))

(defn retry
  "指定された回数まで操作をリトライします。

     options:
     - :max-attempts - default 3
     - :backoff-ms - default 1000
     - :backoff-factor - default 2
     - :jitter - default 0.1
     - :retryable? - true

     Example:
     (retry #(http-call \"https://example.com\")
            {:max-attempts 5
             :backoff-ms 500
             :backoff-factor 1.5
             :retryable? #(contains? #{500 503} (:status %))})
    "
  [f & [{:keys [max-attempts backoff-ms backoff-factor jitter retryable?] :or {max-attempts 3 backoff-ms 1000 backoff-factor 2 jitter 0.1 retryable? (constantly true)}}]]

  (fn [& args]
    (loop [attempts 1
           current-backoff backoff-ms]
      (let [result (apply f args)]
        (cond
          (success? result) result
          (>= attempts max-attempts) result
          :else (do
                  (let [jitter-amount (* current-backoff jitter (- (rand) 0.5))
                        sleep-ms (+ current-backoff jitter-amount)]
                    (<!! (timeout (long sleep-ms))))
                  (recur (inc attempts) (* current-backoff backoff-factor))))))))