(ns otter.operations)

['retain 4 ['insert :x] ['delete 4]]
[:retain 4 [:insert :x] [:delete 4]]

(defrecord retain []) ;; range or tree
(defrecord insert []) ;; value or values?
(defrecord delete []) ;; range or tree; add deleted values?
(defrecord replace []) ;; can be expressed as pair of insert and delete
(defrecord init [])

(retain 4 [(delete :val [1 2 3])]
        {"x" [(insert :val1 :val2)
              (replace (delete :val1) (insert :val2))]})

(retain 4) ;; range
(insert :value) ;; (insert :value1 :value2
(delete :value) ;; (delete :value1 :value2)
(replace :value :new-value)

(insert [(target 1)])
(insert {})
(insert :value)
;; replace xform replace: only one will be applied (different from delete xform insert)
(replace :value :new-value) ;; replace-value
(replace [(retain 4)] {}) ;; replace-tree
(retain []) ;; retain-tree
(retain {}) ;; retain-tree
(retain 4) ;; retain-range
(delete 4) ;; delete-range
(delete [(source 1)])

;; context based operations: do not need to use retain within retain, insert within insert etc.
;; operations only convey context
;; for ease of use, can wrap all non-wrapped items inside operations (but do we need to?)
;; source/target can be implemented at a later time

;; (delta {"something" (insert :value})
;;(dsl [4 {"something" (i :value)} (d) (d 4) 5])
;; (retain [(retain 5) (retain {}) (retain 5)])

(defn join []
  )

(defn split []
  )

(defn length []
  )

;; removes all unnecessary operation wrappers and joins operations if possible
(defn strip []
  )

;; wrapps all values in their contextual operations for ease of processing
(defn swell []
  )


(defn seq-op-type [s]
  (if (symbol? (first s))
    (first s)
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
    (sequential? val) (seq-op-type val)
    :else (panic "invalid root value")))
