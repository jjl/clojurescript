(ns cljs.stm)

(defonce ^:dynamic *transaction* nil)

(deftype Ref
  [state meta validator watches]
  Object
  (equiv [this other] (-equiv this other))

  IEquiv
  (-equiv [this other] (identical? this other))

  IDeref
  (-deref [] state)

  IHash
  (-hash [this] (goog/getUid this))

  IWithMeta
  (-with-meta [this meta]
    (Ref. state meta validator watches))

  IMeta
  (-meta [_] meta)

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key))))

(defn ref?
  "True if ref is a ref"
  [ref]
  (instance? Ref ref))

(defn -require-ref [ref]
  (when-not (ref? ref)
    (throw (ex-info "Not a ref!" {:ref ref}))))

(defn -empty-transaction []
  (let [validator (every-pred vector? (partial every? vector?))]
    (atom [] {:validator validator})))

(defn -update-ref [ref old new]
  (swap! *transaction* conj [ref old new]))

(defn in-transaction?
  "True if we are currently in a transaction or else nil"
  []
  (when *transaction*
    (when-not (atom? *transaction*)
      (throw (ex-info "Invalid transaction!" {:*transaction* *transaction*})))
    true))

(defn -require-transaction []
  (when-not (in-transaction?)
    (throw (ex-info "Not in a transaction!" {}))))

(defn ref
  "Creates and returns a Ref with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will become the metadata on the
  ref. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception. validate-fn will be called on
  transaction commit, when all refs have their final values."
  ([x] (Ref. x))
  ([x & {:keys [meta validator]}]
   (as-> (Ref. x) $
     (if meta (with-meta $ meta) $)
     (do (when validator
           (set-validator! $ validator))
         $))))

(defn -commit []
  (require-transaction)
  (doseq [[ref old-val new-val] @*transaction*]
    (when-let [vfn (get-validator ref)]
      (let [ret (try (vfn @ref)
                     (catch e nil))]
        (when-not ret
          (throw (ex-info "Invalid reference state"
                          {:ref ref :old old-val :new new-val})))))))

(defn -revert []
  (require-transaction)
  ;; revert in reverse order
  (doseq [[ref old-val _] (rseq @*transaction*)]
    (set! (.-state ref) old-val)))

(defmacro sync
  "transaction-flags => TBD, pass nil for now

  Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  [flags-ignored-for-now & exprs]
  (letfn [(f [] ~@exprs)]
    `(if (in-transaction?)
       (~f)
       (binding [*transaction* (-empty-transaction)]
         (try
           (~f)
           (-commit)
           (catch e
             (-revert)))))))

(defmacro dosync
  "Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  [& exprs]
  `(sync nil ~@exprs))

(defmacro io!
  "If an io! block occurs in a transaction, throws an exception, else
  runs body in an implicit do. If the first expression in body is a
  literal string, will use that as the exception message."
  [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (next body) body)]
    `(if (in-transaction?)
       (throw (ex-info ~(or message "I/O in transaction")))
       (do ~@body))))

(defn ensure
  "Must be called in a transaction. Protects the ref from modification
  by other transactions.  Returns the in-transaction-value of
  ref. Allows for more concurrency than (ref-set ref @ref)"
  [ref]
  (require-transaction)
  (require-ref ref)
  @ref)

(defn ref-set
  "Must be called in a transaction. Sets the value of ref.
  Returns val."
  [ref val]
  (require-transaction)
  (require-ref ref)
  (let [old-val (.-state ref)]
    (set! (.-state ref) val)
    (-update-ref ref old-val val)))

(defn alter
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref."
  [ref fun & args]
  (->> (apply fun @ref args)
       (ref-set ref)))

(def commute "Synonym for alter. See alter" alter)
