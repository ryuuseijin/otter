(ns otter.invert
  (:require [otter.operations :as op]))

(defmulti invert        op/op-type-as-root)
(defmulti invert-in-seq op/op-type-in-seq)

;; no special handling for sequences, just call invert on the operation,
;; except for :retain-range which is returned as-is
(defmethod invert-in-seq :retain-map [op] (invert op))
(defmethod invert-in-seq :retain-seq [op] (invert op))
(defmethod invert-in-seq :retain     [op] (invert op))
(defmethod invert-in-seq :insert     [op] (invert op))
(defmethod invert-in-seq :delete     [op] (invert op))
(defmethod invert-in-seq :retain-range [op] op)

(defn invert-seq [ops]
  (into [] (map invert-in-seq) ops))

(defn invert-map [op-map]
  (into {} (map-vals invert-op) op-map))

(defmethod invert :retain-map [op]
  (invert-map op))

(defmethod invert :retain-seq [op]
  (invert-seq op))

(defmethod invert :retain [op]
  (cons 'retain (invert-seq (next op))))

(defmethod invert :insert [op]
  (cons 'delete (next op)))

(defmethod invert :delete [op]
  (cons 'insert (next op)))

(defmethod invert :replace [op]
  (cons (first op) (nth 2 op) (second op)))
