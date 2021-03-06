(ns otter.delta
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(declare optimize)

(defmulti optimize-ops-in-seq (fn [prevs op]
                                (:type op)))

(defn skip-op [prevs op]
  prevs)

(defn keep-op [prevs op]
  (conj prevs op))

(defn maybe-join [prevs op]
  (cond
    (= (:type op) (:type (peek prevs)))
    (peek-replace prevs op/join-ops op)

    (zero? (op/op-length op))
    (skip-op prevs op)
    
    :else
    (keep-op prevs op)))

(defmethod optimize-ops-in-seq :insert-values [prevs op]
  (maybe-join prevs op))

(defmethod optimize-ops-in-seq :retain-range [prevs op]
  (maybe-join prevs op))

(defmethod optimize-ops-in-seq :retain-subtree [prevs op]
  (keep-op prevs (optimize op)))

(defmethod optimize-ops-in-seq :delete-range [prevs op]
  (maybe-join prevs op))

(defmethod optimize-ops-in-seq :replace-value [prevs op]
  (keep-op prevs op))

(defmethod optimize-ops-in-seq :mark [prevs op]
  (keep-op prevs op))

(defn optimize-seq [op-seq]
  (let [r (reduce optimize-ops-in-seq [] op-seq)]
    (cond-> r (= :retain-range (:type (peek r))) pop)))

(defn optimize-map [op-map]
  (->> op-map
       (into {} (comp (map (fn [[k op]]
                             [k (optimize op)]))
                      (filter (fn [[k op]]
                                (not= :retain-range (:type op))))))))


(defn optimize [op]
  (when-not (= 1 (op/op-length op))
    (panic "operations on map values or root nodes must have a length of 1"))
  (case (:type op)
    :retain-subtree
    (cond
      (sequential? (:subtree op))
      (let [r (optimize-seq (:subtree op))]
        (if (seq r)
          (assoc op :subtree r)
          op/retain))

      (map? (:subtree op))
      (let [r (optimize-map (:subtree op))]
        (if (seq r)
          (assoc op :subtree r)
          op/retain)))
    ;;else
    op))
