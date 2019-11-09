(ns otter.materialize
  (:require [otter.utils :refer :all]
            [otter.operations :as op]))

(defmulti materialize-in-map (fn [m k op] (:type op)))
(defmulti materialize-in-seq (fn [prevs nexts op] (:type op)))

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
            (when-not (= 1 (op/op-length op))
              (panic "operations on map values must have a length of exactly 1"))
            (materialize-in-map m k op))
          m op-map))

(defn materialize [tree delta]
  (-> {}
      ;; a nil root node is interpreted as a non-existing tree to
      ;; allow insert-values, which is in contrast to nil map values
      ;; where insert-values is not allowed and replace-value has to
      ;; be used instead
      (cond-> (some? tree) (assoc :k tree))
      (materialize-map {:k (:root-op delta)})
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

(defmethod materialize-in-seq :insert-values [prevs nexts op]
  [(into prevs (:values op))
   nexts])

(defmethod materialize-in-seq :retain-range [prevs nexts op]
  (when (< (count (take (:retain-length op) nexts))
           (:retain-length op))
    (panic "retain-range can only be used to retain existing values"))
  [(into prevs (take (:retain-length op) nexts))
   (drop (:retain-length op) nexts)])

(defmethod materialize-in-seq :retain-subtree [prevs nexts op]
  [(conj prevs (materialize-subtree (first nexts) (:subtree op)))
   (next nexts)])

(defmethod materialize-in-seq :delete-range [prevs nexts op]
  (when (< (count (take (:delete-length op) nexts))
           (:delete-length op))
    (panic "delete-range can only be used to delete existing values"))
  [prevs
   (drop (:delete-length op) nexts)])

(defmethod materialize-in-seq :replace-value [prevs nexts op]
  (when-not (seq nexts)
    (panic "replace-value can only be used to replace existing values"))
  [(conj prevs (:value op))
   (next nexts)])

(defmethod materialize-in-seq :mark [prevs nexts op]
  [prevs nexts])

;; ----- materialize-in-map

(defmethod materialize-in-map :insert-values [m k op]
  (when (contains? m k)
    (panic "insert-values can only be used to insert new map entries or nil root nodes"))
  (assoc m k (first (:values op))))

(defmethod materialize-in-map :retain-range [m k op]
  ;; WARNING: we allow retain-range on non-existent map values or nil
  ;; root nodes, but in an empty sequence or at the end of a sequence we
  ;; throw an error. This is more convenient to allow retain-range to be
  ;; used as a no-op when no changes are made to a tree.
  ;; XX maybe we should only allow it in case of a nil root node, but
  ;;    not for non-existing map values to be more consistent with how it
  ;;    works for sequences
  m)

(defmethod materialize-in-map :retain-subtree [m k op]
  (assoc m k (materialize-subtree (get m k) (:subtree op))))

(defmethod materialize-in-map :delete-range [m k op]
  (when-not (contains? m k)
    (panic "delete-range can only be used with existing map entries or non-nil root nodes"))
  (dissoc m k))

(defmethod materialize-in-map :replace-value [m k op]
  (when-not (contains? m k)
    (panic "replace-value can only be used to replace existing map entries or non-nil root nodes"))
  (assoc m k (:value op)))

(defmethod materialize-in-map :mark [m k op]
  (panic "unable to mark a map value or a root node"))

