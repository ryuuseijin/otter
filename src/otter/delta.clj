(ns otter.delta
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(declare optimize-op)

(defmulti optimize-ops-in-seq (fn [prevs op]
                                (:type op)))

(defn skip-op [prevs op]
  prevs)

(defn keep-op [prevs op]
  (conj prevs op))

(defn maybe-join [joinable-attr-kw zero-width? join-op prevs op]
  (cond
    (= (:type op) (:type (peek prevs)))
    (-> (pop prevs)
        (conj (update (peek prevs) joinable-attr-kw join-op (joinable-attr-kw op))))

    (zero-width? (joinable-attr-kw op))
    (skip-op prevs op)
    
    :else
    (keep-op prevs op)))

(defmethod optimize-ops-in-seq :insert-values [prevs op]
  (maybe-join :values empty? into prevs op))

(defmethod optimize-ops-in-seq :retain-range [prevs op]
  (maybe-join :retain-length zero? + prevs op))

(defmethod optimize-ops-in-seq :retain-subtree [prevs op]
  (keep-op prevs (optimize-op op)))

(defmethod optimize-ops-in-seq :delete-range [prevs op]
  (maybe-join :delete-length zero? + prevs op))

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
                             [k (optimize-op op)]))
                      (filter (fn [[k op]]
                                (not= :retain-range (:type op))))))))


(defn optimize-op [op]
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

(defn optimize [delta]
  (optimize-op (:root-op delta)))
