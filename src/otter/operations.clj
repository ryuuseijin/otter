(ns otter.operations
  (:require [otter.utils :refer :all]))

(defn seq-op-type [s]
  (if (symbol? (first s))
    (keyword (first s))
    :retain-subtree))

(defn op-type [val]
  (cond
    (number? val)     :retain-range
    (map? val)        :retain-subtree
    (sequential? val) (seq-op-type val)
    :else (panic "unknown operation type")))

(def values next)

(defn replaced-value [replace-op]
  (first (values replace-op)))

(defn replacement-value [replace-op]
  (second (values replace-op)))

(defn unwrap-retain-in-seq [ops]
  (concat (values (first ops)) ops))

(defn explicit-retain? [op]
  (and (sequential? op)
       (= 'retain (first op))))

(defn unwrap-retain [op]
  (assert (= 1 (count (values op)))
          "encountered multiple values in a root (retain ...) op")
  (first (values op)))

(defmulti length op-type)

(defmethod length :insert [op]
  (count (values op)))

(defmethod length :retain [op]
  (transduce (map length) + (values op)))

(defmethod length :retain-range [op]
  op)

(defmethod length :retain-subtree [op]
  1)

(defmethod length :delete [op]
  (count (values op)))

(defmethod length :replace [op]
  1)

(defn compare-numeric-length [a b]
  (cond
    (< a b) :smaller
    (> a b) :larger
    :else   :equal))

(defn compare-values-length
  "Like `compare-numeric-length` but for operations with values.
  Will count only as many of each of the operations values as necessary to do the comparsion.
  This is useful to allow algorithms like `xform` and `compose` to complete in linear time by avoiding
  to repeatedly count the larger of two operations that are compared and split."
  [a b]
  ;; XX can be optimized if a or b or both are vectors
  (loop [a-vals (seq (values a))
         b-vals (seq (values b))]
    (cond
      (and a-vals b-vals)
      (recur (next a-vals) (next b-vals))
      a-vals :larger
      b-vals :smaller
      :else  :equal)))

(defn compare-left-numeric-length
  "Like `compare-numeric-length` but for comparing a numeric value with the length of a operation with values."
  [a-num b]
  (compare-numeric-length
   a-num
   ;; XX can be optimized if b is a vector
   (reduce (fn [r op]
             ;; count as few as possible values without affecting the result
             (if (< a-num r)
               (reduced r)
               (inc r)))
           0 (values b))))

(defn compare-right-numeric-length
  "The inverse of compare-left-numeric-length"
  [a b-num]
  (let [result (compare-left-numeric-length b-num a)]
    (case result
      :smaller :larger
      :larger  :smaller
      :equal   :equal)))

;; only insert, delete and retain-range are able to be split
(defmulti split (fn [op len] (op-type op)))

(defmethod split :insert [op len]
  [(list* (first op) (take len op))
   (list* (first op) (drop len op))])

(defmethod split :delete [op len]
  [(list* (first op) (take len op))
   (list* (first op) (drop len op))])

(defmethod split :retain-range [op len]
  [len (- op len)])

(defn join [a b]
  ;;XX
  )

(defn strip [op]
  ;;XX
  )

(defn unstrip [op tree]
  ;;XX
  )
