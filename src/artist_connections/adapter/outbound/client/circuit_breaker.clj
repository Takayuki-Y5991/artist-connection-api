(ns artist-connections.adapter.outbound.client.circuit-breaker
  (:require
   [artist-connections.macros.railway :refer [failure failure?]]
   [clojure.core.async :refer [<! >!! chan go-loop timeout]]))


(defn create-circuit-breaker
  "Create a circuit breaker.
  
   options:
     - :failure-threshold - 開回路になるための連続失敗回数の閾値（デフォルト: 5）
     - :reset-timeout-ms - 半開回路状態になるまでの時間（ミリ秒）（デフォルト: 10000）
     - :half-open-calls - 半開回路状態で許可する呼び出し回数（デフォルト: 1）
     - :failure-predicate - 失敗とみなす条件を判断する関数（デフォルト: failure?）
   
     返り値は、関数を受け取りサーキットブレーカーを適用した新しい関数を返す高階関数です。
  "
  [& [{:keys [failure-threshold reset-timeout-ms half-open-calls failure-predicate]
       :or {failure-threshold 5
            reset-timeout-ms 10000
            half-open-calls 1
            failure-predicate failure?}}]]
  (let [state (atom {:status :closed
                     :failures 0
                     :successes 0})
        reset-channel (chan)
        notify-result (fn [result]
                        (if (failure-predicate result)
                          (swap! state update :failures inc)
                          (swap! state (fn [s] (assoc s :failures 0 :successes (inc (:successes s))))))
                        result)

        update-circuit-state (fn []
                               (let [{:keys [status failures successes]} @state]
                                 (cond
                                   (and (= status :closed) (>= failures failure-threshold))
                                   (do
                                     (swap! state assoc :status :open :last-opened (System/currentTimeMillis))
                                     (>!! reset-channel :schedule-reset))

                                   (and (= status :half-open) (>= successes half-open-calls))
                                   (swap! state assoc :status :closed :failures 0 :successes 0))))]
    (go-loop []
      (when (<! reset-channel)
        (<! (timeout reset-timeout-ms))
        (when (= (:status @state) :open)
          (swap! state assoc :status :half-open :successes 0))
        (recur)))

    (fn [f]
      (fn [& args]
        (let [{:keys [status]} @state]
          (case status
            :closed
            (let [result (apply f args)]
              (notify-result result)
              (update-circuit-state)
              result)

            :open
            (failure {:error "Circuit breaker is open"
                      :details "Service is temporarily unavailable"})

            :half-open
            (let [result (apply f args)]
              (notify-result result)
              (update-circuit-state)
              result)))))))