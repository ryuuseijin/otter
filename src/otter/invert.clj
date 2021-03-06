(ns otter.invert
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(defmulti prepare-op-in-seq (fn [op nodes]
                           (:type op)))

(defn prepare [op tree]
  (let [[op' _] (prepare-op-in-seq op [tree])]
    op'))

(defn prepare-seq [ops nodes]
  (first
   (reduce (fn [[r nodes] op]
             (let [[op' nodes'] (prepare-op-in-seq op nodes)]
               [(conj r op') nodes']))
           [[] nodes]
           ops)))

(defn prepare-map [op-map node-map]
  (into {}
        (map (fn [[k op]]
               [k (prepare op (get node-map k))]))
        op-map))

(defn prepare-subtree [subtree node]
  (cond
    (and (sequential? subtree)
         (sequential? node))
    (prepare-seq subtree node)

    (and (map? subtree)
         (map? node))
    (prepare-map subtree node)

    :else (panic "unable to prepare mismatched subtree types")))

(defmethod prepare-op-in-seq :insert-values [op nodes]
  [op
   nodes])

(defmethod prepare-op-in-seq :retain-range [op nodes]
  [op
   (drop (:retain-length op) nodes)])

(defmethod prepare-op-in-seq :retain-subtree [op nodes]
  [(update op :subtree prepare-subtree (first nodes))
   (next nodes)])

(defmethod prepare-op-in-seq :delete-range [op nodes]
  [(assoc op
          :deleted-values (take (:delete-length op) nodes)
          :have-deleted-values? true)
   (drop (:delete-length op) nodes)])

(defmethod prepare-op-in-seq :replace-value [op nodes]
  [(assoc op
          :replaced-value (first nodes)
          :have-replaced-value? true)
   (next nodes)])

(defmethod prepare-op-in-seq :mark [op nodes]
  [op nodes])

(defmulti invert-op :type)

(defn invert-seq [ops]
  (into [] (map invert-op ops)))

(defn invert-map [op-map]
  (into {}
        (map (fn [[k op]]
               [k (invert-op op)]))
        op-map))

(defn invert-subtree [subtree]
  (cond
    (sequential? subtree)
    (invert-seq subtree)
    
    (map? subtree)
    (invert-map subtree)
    
    :else (panic)))

(defmethod invert-op :insert-values [op]
  (op/delete-range (count (:values op))
                   (:values op)))

(defmethod invert-op :retain-range [op]
  op)

(defmethod invert-op :retain-subtree [op]
  (update op :subtree invert-subtree))

(defn panic-unprepared []
  (panic "unable to invert unprepared delta; use (invert delta tree) or (invert (prepare delta tree))"))

(defmethod invert-op :delete-range [op]
  (when-not (:have-deleted-values? op)
    (panic-unprepared))
  (op/insert-values (:deleted-values op)))

(defmethod invert-op :replace-value [op]
  (when-not (:have-replaced-value? op)
    (panic-unprepared))
  (op/replace-value (:replaced-value op) (:value op)))

(defmethod invert-op :mark [op]
  op)

(defn invert
  ([delta] (invert-op delta))
  ([delta tree]
   (-> delta
       (prepare tree)
       (invert))))
