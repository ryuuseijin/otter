(ns otter.normalize
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

;; Normalization rules:
;;
;; For root nodes the numerical range is unncessary and removed.
;;   (retain 1) => (retain)
;;
;; In maps, values act like a root node,
;;   {:key (retain 1 }) => {:key (retain)}
;; retains around subtrees are unwrapped,
;;   {:key (retain []}) => {:key []      }
;;   {:key (retain {}}) => {:key {}      }
;; entries that do nothing are removed.
;;   {:key (retain)} => {}
;;
;; In sequences, retains are unwrapped,
;;   [(retain ...)] => [...]
;; empty subtrees become retains,
;;   [[] {} ...] => [(retain 1) (retain 1) ...]
;; sequence ops are joined
;;   [(retain 1) (retain 1)]  => [(retain 2) ...]
;; operations that do nothing are removed.
;;   [0 (insert) (delete) (retain) ...] => [...]
;; retains are unwrapped
;;   [(retain 2) ...] => [2 ...]
;; the last retain-range in a sequence is redundant
;;   [... 2] => [...]

(declare normalize-rec)

(defn remove-in-seq? [op]
  (case (op/op-type op)
    (:insert
     :retain
     :delete)
    (not (boolean (seq (op/values op))))

    :retain-range   (zero? op)
    :retain-subtree  false
    :replace         false

    ;;else
    false))

(defn remove-in-map? [op]
  (or (remove-in-seq? op)
      (and (= (op/op-type op) :retain-subtree)
           (empty? op))))

(def listlike-op-types #{:insert :retain :delete})
(def joinable-op-types #{:insert :retain :delete :retain-range})

(defn join-ops? [a b] 
  (and (= (op/op-type a) (op/op-type b))
       (joinable-op-types (op/op-type b))) )

(def join-ops-xf
  (let [;; [insert ...] => (insert ...) undo effect of (vec a)
        flush-op #(cond-> % (listlike-op-types (op/op-type %)) (seq))]
    (stateful-mapcat-xf
     (fn
       ([a]
        (when a
          [(flush-op a)]))
       ([a b]
        (cond
          (and a (join-ops? a b))
          (case (op/op-type b)
            ;; (insert A) (insert B) => (insert A B)
            (:insert :retain :delete)
            [(into (vec a) (op/values b))]

            ;; (retain 1) (retain 2) => (retain 3)
            :retain-range
            [(+ a b)])

          ;; cannot join; flush a and make b the new state
          a      [b (flush-op a)]
          ;; no state yet; use b as the new state
          :else  [b]))))))

(def normalize-in-seq-xf
  (mapcat
   (fn [op]
     (case (op/op-type op)
       ;; [(retain ...)] => [...]
       :retain          (op/values op)
       ;; [{}] => [1]
       :retain-subtree  (if (empty? op)
                          [1]
                          [op])
       ;;else
       [op]))))

(defn normalize-seq [ops]
  (into []
        (comp (map normalize-rec)
              normalize-in-seq-xf
              (filter (complement remove-in-seq?))
              join-ops-xf)
        ops))

(defn normalize-in-map [op]
  (cond
    ;; {:key (retain [])} => {:key []}
    (and (= (op/op-type op) :retain)
         (or (sequential? (first (op/values op)))
             (map?        (first (op/values op)))))
    (first (op/values op))

    ;; (retain 1) => (retain)
    (and (= (op/op-type op) :retain)
         (number? (first (op/values op))))
    '(retain)

    :else op))

(defn normalize-map [op-map]
  (into (empty op-map)
        (comp (map-vals (comp normalize-rec
                              normalize-in-map))
              (filter-vals (complement remove-in-map?)))
        op-map))

(defn normalize-rec [op]
  (case (op/op-type op)
    :retain
    (list* (first op) (normalize-seq (op/values op)))

    :retain-subtree
    (cond
      (sequential? op)
      (let [n-ops (normalize-seq op)]
        ;;   [... 2] => [...]
        (cond-> n-ops
          (and (seq n-ops)
               (number? (peek n-ops)))
          pop))

      (map? op)
      (normalize-map op))
    
    ;;else
    op))

(defn normalize [op]
  (let [n-op (normalize-rec op)]
    ;; (retain 1) => (retain)
    (if (and (= (op/op-type n-op) :retain)
             (number? (first (op/values n-op))))
      '(retain)
      n-op)))
