(ns artist-connections.macros.railway
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as async]))

;; -----------------------------------------------------------------------------
;; Result Types
;; -----------------------------------------------------------------------------
;; Railway Oriented Programming (ROP)の基本型。
;; 通常の値、遅延値、非同期値（Future、Promise、core.asyncチャネル）を
;; 一貫した方法で扱うことができます。

;; 基本結果型
(defprotocol RailwayResult
  "Railway結果の基本プロトコル"
  (unwrap [this] "結果の実際の値を取得")
  (success? [this] "成功結果かどうかを判定")
  (failure? [this] "失敗結果かどうかを判定")
  (map-success [this f] "成功の場合に関数fを適用")
  (map-failure [this f] "失敗の場合に関数fを適用")
  (flat-map [this f] "成功の場合に関数fを適用し、結果をフラット化"))

;; 同期成功結果
(defrecord Success [value]
  RailwayResult
  (unwrap [_] value)
  (success? [_] true)
  (failure? [_] false)
  (map-success [_ f] (Success. (f value)))
  (map-failure [this _] this)
  (flat-map [_ f] (f value)))

;; 同期失敗結果
(defrecord Failure [error]
  RailwayResult
  (unwrap [_] error)
  (success? [_] false)
  (failure? [_] true)
  (map-success [this _] this)
  (map-failure [_ f] (Failure. (f error)))
  (flat-map [this _] this))

;; 遅延評価結果
(defrecord Deferred [value-fn result-type]
  RailwayResult
  (unwrap [_] (value-fn))
  (success? [_] (= result-type :success))
  (failure? [_] (= result-type :failure))
  (map-success [this f]
    (if (= result-type :success)
      (Deferred. #(f (value-fn)) :success)
      this))
  (map-failure [this f]
    (if (= result-type :failure)
      (Deferred. #(f (value-fn)) :failure)
      this))
  (flat-map [this f]
    (if (= result-type :success)
      (Deferred. #(f (value-fn)) result-type)
      this)))

;; 非同期結果 (Future/Promise)
(defrecord AsyncResult [promise]
  RailwayResult
  (unwrap [_] @promise)
  (success? [_]
    (let [result @promise]
      (if (satisfies? RailwayResult result)
        (success? result)
        true))) ; 例外が発生しなければ成功と見なす
  (failure? [_]
    (let [result @promise]
      (if (satisfies? RailwayResult result)
        (failure? result)
        false)))
  (map-success [_ f]
    (AsyncResult.
     (promise
      (let [result @promise]
        (if (and (satisfies? RailwayResult result) (success? result))
          (map-success result f)
          (if (instance? Throwable result)
            (->Failure result)
            (->Success (f result))))))))
  (map-failure [_ f]
    (AsyncResult.
     (promise
      (let [result @promise]
        (if (and (satisfies? RailwayResult result) (failure? result))
          (map-failure result f)
          result)))))
  (flat-map [_ f]
    (AsyncResult.
     (promise
      (let [result @promise]
        (if (and (satisfies? RailwayResult result) (success? result))
          (f (unwrap result))
          result))))))

;; core.async結果
(defrecord ChannelResult [channel]
  RailwayResult
  (unwrap [_] (async/<!! channel))
  (success? [_]
    (let [result (async/<!! channel)]
      (if (satisfies? RailwayResult result)
        (success? result)
        true)))
  (failure? [_]
    (let [result (async/<!! channel)]
      (if (satisfies? RailwayResult result)
        (failure? result)
        false)))
  (map-success [_ f]
    (ChannelResult.
     (async/go
       (let [result (async/<! channel)]
         (if (and (satisfies? RailwayResult result) (success? result))
           (map-success result f)
           (if (instance? Throwable result)
             (->Failure result)
             (->Success (f result))))))))
  (map-failure [_ f]
    (ChannelResult.
     (async/go
       (let [result (async/<! channel)]
         (if (and (satisfies? RailwayResult result) (failure? result))
           (map-failure result f)
           result)))))
  (flat-map [_ f]
    (ChannelResult.
     (async/go
       (let [result (async/<! channel)]
         (if (and (satisfies? RailwayResult result) (success? result))
           (f (unwrap result))
           result))))))

;; -----------------------------------------------------------------------------
;; コンストラクタ関数
;; -----------------------------------------------------------------------------

(defn success
  "成功結果を作成します。値はすぐに評価されます。"
  [value]
  (->Success value))

(defn failure
  "失敗結果を作成します。エラーはすぐに評価されます。"
  [error]
  (->Failure error))

(defn success-defer
  "遅延評価される成功結果を作成します。値は実際に必要になるまで計算されません。"
  [value-fn]
  (->Deferred value-fn :success))

(defn failure-defer
  "遅延評価される失敗結果を作成します。エラーは実際に必要になるまで計算されません。"
  [error-fn]
  (->Deferred error-fn :failure))

(defn result->async
  "非同期結果を作成します。Future、Promise、またはチャネルから結果を作成できます。"
  [async-value]
  (cond
    (future? async-value) (->AsyncResult async-value)
    (instance? clojure.lang.IPending async-value) (->AsyncResult async-value)
    (instance? clojure.core.async.impl.channels.ManyToManyChannel async-value) (->ChannelResult async-value)
    :else (throw (IllegalArgumentException. "非同期値ではありません"))))

(defn success-future
  "非同期で評価される成功結果を作成します。"
  [value-fn]
  (->AsyncResult (future (success (value-fn)))))

(defn failure-future
  "非同期で評価される失敗結果を作成します。"
  [error-fn]
  (->AsyncResult (future (failure (error-fn)))))

(defn success-go
  "core.asyncのgoブロックで評価される成功結果を作成します。"
  [value-fn]
  (->ChannelResult (async/go (success (value-fn)))))

(defn failure-go
  "core.asyncのgoブロックで評価される失敗結果を作成します。"
  [error-fn]
  (->ChannelResult (async/go (failure (error-fn)))))

;; -----------------------------------------------------------------------------
;; ユーティリティ関数
;; -----------------------------------------------------------------------------

(defn railway-result?
  "値がRailwayResult型かどうかを判定します。"
  [value]
  (satisfies? RailwayResult value))

(defn ensure-railway
  "値がRailwayResultでない場合、Successにラップします。"
  [value]
  (if (railway-result? value)
    value
    (success value)))

(defn result->force
  "遅延または非同期の結果を強制的に評価します。"
  [result]
  (cond
    (instance? Deferred result) (if (success? result)
                                  (success (unwrap result))
                                  (failure (unwrap result)))
    (instance? AsyncResult result) (let [value (unwrap result)]
                                     (if (railway-result? value)
                                       value
                                       (success value)))
    (instance? ChannelResult result) (let [value (unwrap result)]
                                       (if (railway-result? value)
                                         value
                                         (success value)))
    :else result))

;; -----------------------------------------------------------------------------
;; Core Railway Operations
;; -----------------------------------------------------------------------------

(defmacro |>
  "成功値を関数にスレッドします。入力が成功の場合のみ関数を適用し、
   そうでない場合は失敗をそのまま通過させます。"
  [value & fns]
  `(let [result# (ensure-railway ~value)]
     (-> result#
         ~@(map (fn [f] `(map-success (fn [x#] (~f x#)))) fns))))

(defmacro |-|
  "エラー値を関数にスレッドします。入力が失敗の場合のみ関数を適用し、
   そうでない場合は成功をそのまま通過させます。"
  [value & fns]
  `(let [result# (ensure-railway ~value)]
     (-> result#
         ~@(map (fn [f] `(map-failure (fn [x#] (~f x#)))) fns))))

(defmacro >-<
  "成功/失敗に基づいて処理を分岐します。"
  [value success-fn failure-fn]
  `(let [result# (ensure-railway ~value)]
     (if (success? result#)
       (~success-fn (unwrap result#))
       (~failure-fn (unwrap result#)))))

(defmacro <|>
  "失敗時に代替を試みます。値が失敗の場合、代替を評価して返します。
   そうでない場合、元の成功をそのまま返します。"
  [value alternative]
  `(let [result# (ensure-railway ~value)]
     (if (failure? result#)
       ~alternative
       result#)))

;; -----------------------------------------------------------------------------
;; Validation Utilities
;; -----------------------------------------------------------------------------

(defn validate
  "値をspecに対して検証し、railway結果を返します。
   有効な場合は値とともに成功を返し、そうでない場合は詳細なエラー情報とともに失敗を返します。"
  [spec value]
  (if (s/valid? spec value)
    (success value)
    (failure {:error "検証に失敗しました"
              :details (s/explain-str spec value)})))

(defn validate-defer
  "遅延評価される検証を作成します。値とspecが必要になるまで評価されません。"
  [spec value-fn]
  (success-defer #(if (s/valid? spec (value-fn))
                    (value-fn)
                    (throw (ex-info "検証に失敗しました"
                                    {:error "検証に失敗しました"
                                     :details (s/explain-str spec (value-fn))})))))

(defn validate-async
  "非同期で実行される検証を作成します。"
  [spec value]
  (success-future #(if (s/valid? spec value)
                     value
                     (throw (ex-info "検証に失敗しました"
                                     {:error "検証に失敗しました"
                                      :details (s/explain-str spec value)})))))

;; -----------------------------------------------------------------------------
;; Error Handling
;; -----------------------------------------------------------------------------

(defmacro !>
  "Railway指向のtry/catch。ボディを実行し、結果とともに成功を返します。
   例外が発生した場合、エラー詳細とともに失敗を返します。"
  [& body]
  `(try
     (success (do ~@body))
     (catch Throwable e#
       (failure {:error "予期しないエラー"
                 :details (.getMessage e#)
                 :exception e#}))))

(defmacro !>defer
  "遅延評価されるRailway指向のtry/catch。"
  [& body]
  `(success-defer
    (fn []
      (try
        (do ~@body)
        (catch Throwable e#
          (throw (ex-info "予期しないエラー"
                          {:error "予期しないエラー"
                           :details (.getMessage e#)
                           :exception e#})))))))

(defmacro !>future
  "非同期で実行されるRailway指向のtry/catch。"
  [& body]
  `(success-future
    (fn []
      (try
        (do ~@body)
        (catch Throwable e#
          (throw (ex-info "予期しないエラー"
                          {:error "予期しないエラー"
                           :details (.getMessage e#)
                           :exception e#})))))))

(defmacro !>go
  "core.asyncのgoブロックで実行されるRailway指向のtry/catch。"
  [& body]
  `(success-go
    (fn []
      (try
        (do ~@body)
        (catch Throwable e#
          (throw (ex-info "予期しないエラー"
                          {:error "予期しないエラー"
                           :details (.getMessage e#)
                           :exception e#})))))))

;; -----------------------------------------------------------------------------
;; Composition Utilities
;; -----------------------------------------------------------------------------

(defn |+
  "複数の関数をrailwayパターンでチェーンします。各関数をシーケンスで適用し、
   最初に遭遇した失敗を返す新しい関数を返します。"
  [& fs]
  (fn [x]
    (reduce (fn [acc f]
              (if (success? acc)
                (try
                  (let [result (f (unwrap acc))]
                    (ensure-railway result))
                  (catch Throwable e
                    (failure e)))
                acc))
            (ensure-railway x)
            fs)))

(defn |+defer
  "遅延評価される複数の関数のチェーン。"
  [& fs]
  (fn [x]
    (success-defer
     #(reduce (fn [acc f]
                (if (success? (ensure-railway acc))
                  (try
                    (let [result (f (unwrap (ensure-railway acc)))]
                      (unwrap (ensure-railway result)))
                    (catch Throwable e
                      (throw e)))
                  (throw (ex-info "前のステップで失敗" {:error acc}))))
              x
              fs))))

(defn |+future
  "非同期で実行される複数の関数のチェーン。"
  [& fs]
  (fn [x]
    (success-future
     #(reduce (fn [acc f]
                (if (success? (ensure-railway acc))
                  (try
                    (let [result (f (unwrap (ensure-railway acc)))]
                      (unwrap (ensure-railway result)))
                    (catch Throwable e
                      (throw e)))
                  (throw (ex-info "前のステップで失敗" {:error acc}))))
              x
              fs))))

;; -----------------------------------------------------------------------------
;; Railway Combinators
;; -----------------------------------------------------------------------------

(defmacro either
  "述語に基づいて2つの関数のいずれかを適用する関数を作成します。"
  [pred then else]
  `(fn [x#]
     (if (~pred x#)
       (~then x#)
       (~else x#))))

(defmacro guard
  "述語に合格した場合に成功し、そうでない場合に失敗する関数を作成します。"
  [pred error-msg]
  `(fn [x#]
     (if (~pred x#)
       (success x#)
       (failure {:error ~error-msg}))))

(defmacro attempt
  "例外をキャッチして提供されたハンドラで処理する関数を作成します。"
  [f error-handler]
  `(fn [x#]
     (try
       (~f x#)
       (catch Throwable e#
         (~error-handler e#)))))

;; -----------------------------------------------------------------------------
;; 非同期ユーティリティ
;; -----------------------------------------------------------------------------

(defn all>
  "複数の結果が全て成功した場合にのみ成功する結果を返します。"
  [results]
  (reduce (fn [acc result]
            (if (success? acc)
              (if (success? result)
                (map-success acc #(conj % (unwrap result)))
                result)
              acc))
          (success [])
          results))

(defn any>
  "少なくとも1つの結果が成功した場合に成功する結果を返します。"
  [results]
  (reduce (fn [acc result]
            (if (failure? acc)
              (if (success? result)
                result
                acc)
              acc))
          (failure "全ての操作が失敗しました")
          results))

(defmacro with-timeout
  "指定された時間内に操作が完了しない場合、タイムアウトエラーを返します。"
  [ms operation]
  `(let [promise# (promise)
         result# (future
                   (try
                     (deliver promise# ~operation)
                     (catch Throwable e#
                       (deliver promise# (failure e#)))))]
     (try
       (let [result# (deref promise# ~ms ::timeout)]
         (if (= result# ::timeout)
           (failure {:error "操作がタイムアウトしました"
                     :timeout-ms ~ms})
           result#))
       (catch Throwable e#
         (failure e#)))))

(defn retry
  "指定された回数だけ操作の実行を試みます。"
  [max-attempts operation & {:keys [delay] :or {delay 0}}]
  (loop [attempts 1
         last-error nil]
    (if (> attempts max-attempts)
      (failure {:error "最大再試行回数に達しました"
                :attempts max-attempts
                :last-error last-error})
      (let [result (try
                     (operation)
                     (catch Throwable e
                       (failure e)))]
        (if (success? result)
          result
          (do
            (when (> delay 0)
              (Thread/sleep delay))
            (recur (inc attempts) (unwrap result))))))))

;; -----------------------------------------------------------------------------
;; デバッグユーティリティ
;; -----------------------------------------------------------------------------

(defmacro |>log
  "処理の各ステップをロギングします。"
  [value label & fns]
  `(let [result# (ensure-railway ~value)
         log-fn# (fn [r# stage#]
                   (println (str ~label " [" stage# "]: " r#))
                   r#)]
     (log-fn# result# "入力")
     (-> result#
         ~@(map (fn [f]
                  `(map-success (fn [x#]
                                  (let [result# (~f x#)]
                                    (log-fn# (ensure-railway result#) "出力")
                                    result#))))
                fns))))

(defmacro |>time
  "操作の実行時間を測定します。"
  [value & fns]
  `(let [result# (ensure-railway ~value)
         start-time# (System/currentTimeMillis)]
     (-> result#
         ~@(map (fn [f]
                  `(map-success (fn [x#]
                                  (let [result# (~f x#)
                                        end-time# (System/currentTimeMillis)
                                        elapsed# (- end-time# start-time#)]
                                    (println (str "操作が " elapsed# "ms で完了しました"))
                                    result#))))
                fns))))

;; -----------------------------------------------------------------------------
;; 変換ユーティリティ
;; -----------------------------------------------------------------------------

(defn result->either
  "Railway結果をEitherパターン（[::left error]または[::right value]）に変換します。"
  [result]
  (if (success? result)
    [::right (unwrap result)]
    [::left (unwrap result)]))

(defn either->result
  "Eitherパターンの値をRailway結果に変換します。"
  [[tag value]]
  (case tag
    ::right (success value)
    ::left (failure value)))

(defn result->promise
  "Railway結果をPromiseに変換します。"
  [result]
  (let [p (promise)]
    (deliver p (result->force result))
    p))

(defn result->channel
  "Railway結果をcore.asyncチャネルに変換します。"
  [result]
  (let [ch (async/chan 1)]
    (async/put! ch (result->force result))
    ch))

;; -----------------------------------------------------------------------------
;; トランザクション制御
;; -----------------------------------------------------------------------------

(defmacro with-transaction
  "トランザクション内で操作を実行します。失敗した場合はロールバックを試みます。"
  [tx-begin tx-commit tx-rollback & body]
  `(let [_ (~tx-begin)]
     (try
       (let [result# (do ~@body)]
         (if (success? result#)
           (do
             (~tx-commit)
             result#)
           (do
             (~tx-rollback)
             result#)))
       (catch Throwable e#
         (~tx-rollback)
         (failure {:error "トランザクションエラー"
                   :details (.getMessage e#)
                   :exception e#})))))