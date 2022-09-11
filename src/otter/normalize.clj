(ns otter.normalize
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(defn nop? [op]
  (case (op/op-type op)
    (:insert
     :retain
     :delete)
    (boolean (seq (op/values op)))

    :retain-range   (zero? op)
    :retain-subtree (empty? op)
    :replace         false

    ;;else
    false))

(declare normalize-seq
         normalize)

(defn normalize-in-seq-xf [rf]
  (let [prev (volatile! nil)]
    (fn normalize-rec
      ([] (rf))
      ([result]
       (rf (cond-> result @prev (rf @prev))))
      ([result op]
       (if (and @prev (= (op/op-type @prev)
                         (op/op-type op)))
         (vswap! @prev into (op/values op) )
         (case (op/op-type op)
           (:insert :delete)
           (vreset! @prev (vec op))

           :retain
           (reduce normalize-rec result (normalize-seq (op/values op)))

           ;;else
           (rf result op)))))))

(defn normalize-seq [ops]
  (into []
        (comp (map normalize)
              (filter (complement nop?))
              normalize-in-seq-xf)
        ops))

(defn normalize-in-map [op]
  (let [n-op (normalize op)]
    (if (= :retain (op/op-type n-op))
      (let [child-op (first (op/values n-op))]
        (cond
          (or (seq? child-op)
              (map? child-op))
          child-op

          (number? child-op)
          '(retain)))
      n-op)))

(defn normalize-map [op-map]
  (into (empty op-map)
        (comp (map-vals normalize-in-map)
              (filter-vals (complement nop?)))
        op-map))

(defmulti normalize op/op-type)

(defn normalize [op]
  (case (op/op-type op)
    :retain
    (if-let [n-ops (seq (normalize-seq (op/values op)))]
      (list* (first op) n-ops)
      '(retain))

    :retain-subtree
    (cond
      (seq? op) (normalize-seq op)
      (map? op) (normalize-map op))

    ;;else
    op))
