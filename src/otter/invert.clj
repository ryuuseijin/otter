(ns otter.invert
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(defmulti invert op/op-type)

(defn invert-seq [ops]
  (into [] (map invert) ops))

(defn invert-map [op-map]
  (into {} (map-vals invert) op-map))

(defmethod invert :insert [op]
  (cons 'delete (next op)))

(defmethod invert :retain [op]
  (cons 'retain (invert-seq (next op))))

(defmethod invert :retain-range [op]
  op)

(defmethod invert :retain-subtree [op]
  (cond
    (sequential? op) (invert-seq op)
    (map? op       ) (invert-map op)))

(defmethod invert :delete [op]
  (cons 'insert (next op)))

(defmethod invert :replace [op]
  (cons (first op)
        (second (op/values op))
        (first (op/values op))))
