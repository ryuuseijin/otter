(ns otter.invert
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(defmulti invert-in-seq (fn [op nodes] (:type op)))

(defn invert-op [op node]
  (let [[op' _] (invert-in-seq op [node])]
    op'))

(defn invert-seq [ops nodes]
  (let [[r _] (reduce (fn [[r nodes] op]
                        (let [[op' nodes] (invert-in-seq op nodes)]
                          [(conj r op') nodes]))
                      [[] nodes]
                      ops)]
    r))

(defn invert-map [op-map node-map]
  (into {}
        (map (fn [[k op]]
               [k (invert-op op (get node-map k))]))
        op-map))

(defn invert-subtree [subtree tree]
  (cond
    (and (sequential? subtree)
         (sequential? tree))
    (invert-seq subtree tree)
    
    (and (map? subtree)
         (map? tree))
    (invert-map subtree tree)
    
    :else (panic "unable to invert retain-subtree with mismatched subtree type")))

(defmethod invert-in-seq :insert-values [op nodes]
  [(op/delete-range (count (:values op)))
   nodes])

(defmethod invert-in-seq :retain-range [op nodes]
  [op
   (drop (:retain-length op) nodes)])

(defmethod invert-in-seq :retain-subtree [op nodes]
  [(assoc op :subtree (invert-subtree (:subtree op) (first nodes)))
   (next nodes)])

(defmethod invert-in-seq :delete-range [op nodes]
  [(op/insert-values (take (:delete-length op) nodes))
   (drop (:delete-length op) nodes)])

(defmethod invert-in-seq :replace-value [op nodes]
  [(op/replace-value (first nodes))
   (next nodes)])

(defmethod invert-in-seq :mark [op nodes]
  [op
   nodes])

(defn invert [delta tree]
  (invert-op (:root-op delta) tree))
