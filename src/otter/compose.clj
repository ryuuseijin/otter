(ns otter.compose
  (:require [otter.core :as ot]
            [otter.operations :as op]
            [otter.materialize :refer [materialize]]
            [otter.utils :refer :all]))

(defmulti compose-in-seq_any_any
  (fn [na nb]
    [(:type (first na))
     (:type (first nb))]))

(defmulti compose-in-map_any_any
  (fn [a b]
    [(:type a)
     (:type b)]))

(defn compose-ops [a b]
  (when-not (and (= 1 (op/op-length a)
                    1 (op/op-length b)))
    (panic "operations on tree roots and map values must have a length of exactly 1"))
  (compose-in-map_any_any a b))

(defn compose
  ([] (ot/delta op/retain))
  ([delta-a delta-b]
   (ot/delta (compose-ops (:root-op delta-a) (:root-op delta-b)))))

(defn compose-maps [map-a map-b]
  (->> map-b
       (map (fn [[k op-b]]
              [k (if-let [op-a (get map-a k)]
                   (compose-ops op-a op-b)
                   op-b)]))
       (into map-a)))

(defn compose-seqs [ops-a ops-b]
  (loop [r []
         na ops-a
         nb ops-b]
    (if (or (seq na) (seq nb)) ;;XX seq should not be necessary since we use next
      (let [[ops na' nb'] (compose-in-seq_any_any na nb)]
        (recur (into r ops) na' nb'))
      r)))

(defn compose-subtrees [subtree-a subtree-b]
  (cond
    (and (map? subtree-a) (map? subtree-b))
    (compose-maps subtree-a subtree-b)

    (and (sequential? subtree-a) (sequential? subtree-b))
    (compose-seqs subtree-a subtree-b)

    :else (panic "unable to compose mismatched subtree types")))

;; -------- compose-in-seq

;; insert-values

(defmethod compose-in-seq_any_any [:insert-values :insert-values] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:insert-values :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      ;; split the insert-values op
      (< (:retain-length b) (count (:values a)))
      [[(op/insert-values (take (:retain-length b) (:values a)))]
       (->> (next na)
            (cons (op/insert-values (drop (:retain-length b) (:values a)))))
       (next nb)]
      ;; split the retain-range op
      (> (:retain-length b) (count (:values a)))
      [[a]
       (next na)
       (->> (next nb)
            (cons (update b :retain-length - (count (:values a)))))]
      ;; exact overlap
      :else
      [[a] (next na) (next nb)])))

;;XX this is similar to :insert-values :retain-range
(defmethod compose-in-seq_any_any [:insert-values :delete-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      ;; split the insert-values op
      (< (:delete-length b) (count (:values a)))
      [[]
       (->> (next na)
            (cons (op/insert-values (drop (:delete-length b) (:values a)))))
       (next nb)]
      ;; split the delete-range op
      (> (:delete-length b) (count (:values a)))
      [[]
       (next na)
       (->> (next nb)
            (cons (update b :delete-length - (count (:values a)))))]
      ;; exact overlap
      :else
      [[] (next na) (next nb)])))

(defmethod compose-in-seq_any_any [:insert-values :retain-subtree] [na nb]
  (let [a (first na)]
    (case (count (:values a))
      ;; ignore zero-length inserts
      0 [[] (next na) nb]
      1 (let [a (first na)
              b (first nb)]
          [[(assoc a :values [(materialize (first (:values a)) (ot/delta b))])]
           (next na)
           (next nb)])
      ;; split the insert-values
      [[]
       (->> (next na)
            (cons (op/insert-values (drop 1 (:values a))))
            (cons (op/insert-values (take 1 (:values a)))))
       nb])))

;;XX similar to :insert-values :retain-subtree
(defmethod compose-in-seq_any_any [:insert-values :replace-value] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (count (:values a))
      ;; ignore the insert
      0 [[]
         (next na)
         (next nb)]
      ;; replace the value to be inserted
      1 [[(assoc a :values [(:value b)])]
         (next na)
         (next nb)]
      ;; split the insert and retry
      [[]
       (->> (next na)
            (cons (op/insert-values (drop 1 (:values a))))
            (cons (op/insert-values (take 1 (:values a)))))
       nb])))

(defmethod compose-in-seq_any_any [:insert-values :mark] [na nb]
  (let [a (first na)
        b (first nb)]
    ;; compose the mark, retry the insert
    [[b] na (next nb)]))

(defmethod compose-in-seq_any_any [:insert-values nil] [na nb]
  ;; compose the insert
  [[(first na)] (next na) nb])

;; retain-range

(defmethod compose-in-seq_any_any [:retain-range :insert-values] [na nb]
  ;; compose the insert, retry the retain
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:retain-range :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      ;; compose a, split b
      (< (:retain-length a) (:retain-length b))
      [[a]
       (next na)
       (->> (next nb)
            (cons (update b :retain-length - (:retain-length a))))]
      ;; compose b, split a
      (> (:retain-length a) (:retain-length b))
      [[b]
       (->> (next na)
            (cons (update a :retain-length - (:retain-length b))))
       (next nb)]
      ;; exact overlap
      :else [[a] (next na) (next nb)])))

(defmethod compose-in-seq_any_any [:retain-range :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (:retain-length a)
      ;; ignore zero-length retains
      0 [[] (next na) nb]
      ;; compose the retain-subtree op
      1 [[b]
         (next na)
         (next nb)]
      ;; split the retain-range op and retry
      [[]
       (->> (next na)
            (cons (update a :retain-length dec))
            (cons (assoc a :retain-length 1)))
       nb])))

(defmethod compose-in-seq_any_any [:retain-range :delete-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      ;; split the delete-length op
      (< (:retain-length a) (:delete-length b))
      [[]
       na
       (->> (next nb)
            (cons (update b :delete-length - (:retain-length a)))
            (cons (assoc b :delete-length (:retain-length a))))]
      ;; split the retain-length op
      (> (:retain-length a) (:delete-length b))
      [[]
       (->> (next na)
            (cons (update a :retain-length - (:delete-length b)))
            (cons (assoc a :retain-length (:delete-length b))))
       nb]
      ;; exact overlap
      :else
      [[b] (next na) (next nb)])))

(defmethod compose-in-seq_any_any [:retain-range :replace-value] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (:retain-length a)
      ;; ignore zero-length retains
      0 [[] (next na) nb]
      ;; 
      1 [[b] (next na) (next nb)]
      ;; split the retain
      [[]
       (->> (next na)
            (cons (update a :retain-length dec))
            (cons (assoc a :retain-length 1)))
       nb])))

(defmethod compose-in-seq_any_any [:retain-range :mark] [na nb]
  ;; compose the mark
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:retain-range nil] [na nb]
  ;; retain-range at the end of a sequence can be omitted
  [[(first na)] (next na) nb])

;; retain-subtree

(defmethod compose-in-seq_any_any [:retain-subtree :insert-values] [na nb]
  ;; compose the insert-values op
  [[(first nb)] na(next nb)])

(defmethod compose-in-seq_any_any [:retain-subtree :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (:retain-length b)
      ;; ignore zero-length retains
      0 [[] na (next nb)]
      ;; compose retain-subtree
      1 [[a] (next na) (next nb)]
      ;; split retain-range
      [[]
       na
       (->> (next nb)
            (cons (update b :retain-length dec))
            (cons (assoc b :retain-length 1)))])))

(defmethod compose-in-seq_any_any [:retain-subtree :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)
        subtree (compose-subtrees (:subtree a) (:subtree b))]
    [[(assoc a :subtree subtree)]
     (next na)
     (next nb)]))

(defmethod compose-in-seq_any_any [:retain-subtree :delete-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (:delete-length b)
      ;; ignore zero-length deletes
      0 [[] na (next nb)]
      ;; delete the subtree
      1 [[b] (next na) (next nb)]
      ;; split the delete
      [[]
       na
       (->> (next nb)
            (cons (update b :delete-length dec))
            (cons (assoc b :delete-length 1)))])))

(defmethod compose-in-seq_any_any [:retain-subtree :replace-value] [na nb]
  ;; replace the subtree
  [[(first nb)] (next na) (next nb)])

(defmethod compose-in-seq_any_any [:retain-subtree :mark] [na nb]
  ;; compose the mark
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:retain-subtree nil] [na nb]
  [[(first na)] (next na) nb])

;; delete-range

(defmethod compose-in-seq_any_any [:delete-range :insert-values] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range :retain-range] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range :retain-subtree] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range :delete-range] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range :replace-value] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range :mark] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:delete-range nil] [na nb]
  [[(first na)] (next na) nb])

;; replace-value

(defmethod compose-in-seq_any_any [:replace-value :insert-values] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:replace-value :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (:retain-length b)
      ;; ignore zero-length retains
      0 [[] na (next nb)]
      ;; compose the replace-value op
      1 [[a] (next na) (next nb)]
      ;; split the retain-range op
      [[]
       na
       (->> (next nb)
            (cons (update b :retain-length dec))
            (cons (assoc b :retain-length 1)))])))

(defmethod compose-in-seq_any_any [:replace-value :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)]
    [[(assoc a :value (materialize (:value a) (ot/delta b)))]
     (next na)
     (next nb)]))

(defmethod compose-in-seq_any_any [:replace-value :delete-range] [na nb]
  (let [b (first nb)]
    (case (:delete-length b)
      ;; ignore zero-length deletes
      0 [[] na (next nb)]
      1 [[b] (next na) (next nb)]
      ;; split delete-range op
      [[]
       na
       (->> (next nb)
            (cons (update b :delete-length dec))
            (cons (assoc b :delete-length 1)))])))

(defmethod compose-in-seq_any_any [:replace-value :replace-value] [na nb]
  ;; the second replace, replaces the first
  [[(first nb)] (next na) (next nb)])

(defmethod compose-in-seq_any_any [:replace-value :mark] [na nb]
  ;; the mark is inserted before the replace
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [:replace-value nil] [na nb]
  [[(first na)] (next na) nb])

;; mark

(defmethod compose-in-seq_any_any [:mark :insert-values] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark :retain-range] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark :retain-subtree] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark :delete-range] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark :replace-value] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark :mark] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq_any_any [:mark nil] [na nb]
  [[(first na)] (next na) nb])

;; end-of-sequence

(defmethod compose-in-seq_any_any [nil :insert-values] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [nil :retain-range] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [nil :retain-subtree] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [nil :delete-range] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [nil :replace-value] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq_any_any [nil :mark] [na nb]
  [[(first nb)] na (next nb)])

;; -------- compose-in-map

;; :insert-values

;; invalid (insert over existing)
(defmethod compose-in-map_any_any [:insert-values :insert-values] [a b]
  b)

(defmethod compose-in-map_any_any [:insert-values :retain-range] [a b]
  a)

;; WARNING: eliminating the delete-range depends on insert-values to be used
;;          only to insert new values, not to replace existing values.
;;          Also see 'invalid (delete non-existing)'
(defmethod compose-in-map_any_any [:insert-values :delete-range] [a b]
  op/retain)

(defmethod compose-in-map_any_any [:insert-values :retain-subtree] [a b]
  (assoc a :values [(materialize (first (:values a)) (ot/delta b))]))

(defmethod compose-in-map_any_any [:insert-values :replace-value] [a b]
  (assoc a :values [(:value b)]))

;; retain-range

;; invalid (insert over existing | retain non-existing)
(defmethod compose-in-map_any_any [:retain-range :insert-values] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-range :retain-range] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-range :retain-subtree] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-range :delete-range] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-range :replace-value] [a b]
  b)

;; retain-subtree

;; invalid (insert over existing | retain non-existing)
(defmethod compose-in-map_any_any [:retain-subtree :insert-values] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-subtree :retain-range] [a b]
  a)

(defmethod compose-in-map_any_any [:retain-subtree :retain-subtree] [a b]
  (assoc a :subtree (compose-subtrees (:subtree a) (:subtree b))))

(defmethod compose-in-map_any_any [:retain-subtree :delete-range] [a b]
  b)

(defmethod compose-in-map_any_any [:retain-subtree :replace-value] [a b]
  b)

;; delete-range

(defmethod compose-in-map_any_any [:delete-range :insert-values] [a b]
  (op/replace-value (first (:values b))))

;; invalid (retain non-existing)
(defmethod compose-in-map_any_any [:delete-range :retain-range] [a b]
  a)

;; invalid (retain non-existing)
(defmethod compose-in-map_any_any [:delete-range :retain-subtree] [a b]
  (panic ":retain-subtree after a :delete-range is invalid"))

;; invalid (delete non-existing)
(defmethod compose-in-map_any_any [:delete-range :delete-range] [a b]
  b)

;; invalid (replace non-existing)
(defmethod compose-in-map_any_any [:delete-range :replace-value] [a b]
  b)

;; replace-value

;; invalid (insert over existing | replace non-existing)
(defmethod compose-in-map_any_any [:replace-value :insert-values] [a b]
  b)

(defmethod compose-in-map_any_any [:replace-value :retain-range] [a b]
  a)

(defmethod compose-in-map_any_any [:replace-value :retain-subtree] [a b]
  (assoc a :value (materialize (:value a) (ot/delta b))))

(defmethod compose-in-map_any_any [:replace-value :delete-range] [a b]
  b)

(defmethod compose-in-map_any_any [:replace-value :replace-value] [a b]
  b)
