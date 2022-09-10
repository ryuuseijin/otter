(ns otter.materialize
  (:require [otter.utils :refer :all]
            [otter.operations :as op]))

(defmulti materialize-in-map (fn [m k op] (op/op-type op)))
(defmulti materialize-in-seq (fn [prevs nexts op] (op/op-type op)))

(defn materialize-seq [s ops]
  (let [[prevs nexts]
        (reduce (fn [[prevs nexts] op]
                  (materialize-in-seq prevs nexts op))
                [[] s]
                ops)]
    ;; retain-range for the rest of the sequence is implied
    (into prevs nexts)))

(defn materialize-map [m op-map]
  (reduce (fn [m [k op]]
            (when-not (= 1 (op/length op))
              (panic "operations on map values must have a length of exactly 1"))
            (materialize-in-map m k op))
          m op-map))

(defn materialize [tree op]
  (-> {}
      ;; a nil root node is interpreted as a non-existing tree to
      ;; allow insert-values, which is in contrast to nil map values
      ;; where insert-values is not allowed and replace-value has to
      ;; be used instead
      (cond-> (some? tree) (assoc :k tree))
      (materialize-map {:k op})
      :k))

(defn materialize-subtree [tree op-subtree]
  (cond
    (and (sequential? tree)
         (sequential? op-subtree))
    (materialize-seq tree op-subtree)

    (and (map? tree)
         (map? op-subtree))
    (materialize-map tree op-subtree)

    :else
    (panic "mismatching subtree types")))

;; ----- materialize-in-seq

(defmethod materialize-in-seq :insert [prevs nexts op]
  [(into prevs (op/values op))
   nexts])

(defmethod materialize-in-seq :retain-range [prevs nexts op]
  [(into prevs (take op nexts))
   (drop op nexts)])

(defmethod materialize-in-seq :retain-subtree [prevs nexts op]
  [(conj prevs (materialize-subtree (first nexts) op))
   (next nexts)])

(defmethod materialize-in-seq :delete [prevs nexts op]
  [prevs
   (drop (op/length op) nexts)])

(defmethod materialize-in-seq :replace [prevs nexts op]
  [(conj prevs (op/replacement-value op))
   (next nexts)])

;; ----- materialize-in-map

(defmethod materialize-in-map :insert [m k op]
  (when (contains? m k)
    (panic "insert-values can only be used to insert new map entries or nil root nodes"))
  (assoc m k (first (op/values op))))

(defmethod materialize-in-map :retain-range [m k op]
  (when (not= 1 op)
    (panic "retain ranges should be exactly 1 for map values"))
  m)

(defmethod materialize-in-map :retain-subtree [m k op]
  (assoc m k (materialize-subtree (get m k) op)))

(defmethod materialize-in-map :delete [m k op]
  (when-not (contains? m k)
    (panic "delete-range can only be used with existing map entries or non-nil root nodes"))
  (dissoc m k))

(defmethod materialize-in-map :replace [m k op]
  (when-not (contains? m k)
    (panic "replace-value can only be used to replace existing map entries or non-nil root nodes"))
  (assoc m k (op/replacement-value op)))
