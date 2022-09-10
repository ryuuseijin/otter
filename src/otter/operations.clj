(ns otter.operations)

(defn retain [& ops]
  (list* 'retain ops))

(defn insert [& ops]
  (list* 'insert ops))

(defn delete [& ops]
  (list* 'delete ops))

(defn replace [old new]
  (list 'replace old new))

(defn join []
  )

(defn split []
  )

(defn length []
  )


(defn seq-op-type [s]
  (if (symbol? (first s))
    (keyword (first s))
    :retain-subtree))

(defn op-type-in-seq [val]
  (cond
    (number? val)     :retain-range
    (map? val)        :retain-subtree
    (sequential? val) (seq-op-type val)
    :else (panic "invalid value in sequence")))

(defn op-type-as-root [val]
  (cond
    ;; we do not allow bare integers as map values/roots for now because they would always be 1
    ;; could reconsider this if it turns out to make things more consistent
    (map? val)        :retain-subtree
    (sequential? val) (let [t (seq-op-type val)]
                        (if (= t :retain)
                          (if (number? (second val))
                            :retain-range
                            t)))
    :else (panic "invalid root value")))

(def values next)

(defn length [op]
  )

(defn split [op]
  )

(defn compare-length [a b]
  )

(defn strip [op]
  )

(defn unstrip [op tree]
  )
