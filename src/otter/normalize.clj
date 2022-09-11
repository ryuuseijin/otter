(ns otter.normalize
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(declare normalize-seq
         normalize)

(defn nop? [op]
  (case (op/op-type op)
    (:insert
     :retain
     :delete)
    (not (boolean (seq (op/values op))))

    :retain-range   (zero? op)
    :retain-subtree (empty? op)
    :replace         false

    ;;else
    false))

(def join-ops-xf
  (let [finalize-op
        (fn [op]
          (case (op/op-type op)
            ;; turn the vector op into a sequence for consistency
            (:insert :retain :delete)
            (seq op)

            ;;else: nothing to do
            op))]
    (stateful-mapcat-xf
     (fn
       ([a] [(finalize-op a)])
       ([a b]
        (if (and a
                 (= (op/op-type a)
                    (op/op-type b))
                 (contains? #{:insert :retain :delete :retain-range}
                            (op/op-type b)))
          (case (op/op-type b)
            ;; join sequences
            (:insert :retain :delete)
            [(into (vec a) (op/values b))]

            ;; join numeric ranges
            :retain-range
            [(+ a b)])
          (if a
            ;; cannot join, flush a and make b the new state
            [b (finalize-op a)]
            ;; no state yet, use b as the new state
            [b])))))))

(defn normalize-seq [ops]
  (into []
        (comp (map normalize)
              (filter (complement nop?))
              join-ops-xf)
        ops))

(defn normalize-in-map [op]
  (let [n-op (normalize op)]
    (if (and (= :retain (op/op-type n-op))
             (or (sequential? (first (op/values n-op)))
                 (map?        (first (op/values n-op)))))
      (first (op/values n-op))
      n-op)))

(defn normalize-map [op-map]
  (into (empty op-map)
        (comp (map-vals normalize-in-map)
              (filter-vals (complement nop?)))
        op-map))

(defn normalize [op]
  (case (op/op-type op)
    :retain
    (let [[child-op :as child-ops] (normalize-seq (op/values op))]
      (if (number? child-op)
        '(retain)
        (list* 'retain child-ops)))

    :retain-subtree
    (cond
      (sequential? op) (normalize-seq op)
      (map?        op) (normalize-map op))

    ;;else
    op))
