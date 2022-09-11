(ns otter.compose
  (:require [otter.operations :as op]
            [otter.utils :refer :all]
            [otter.materialize :refer [materialize]]
            [otter.invert :refer [invert]]))

(defmulti compose-in-seq
  (fn [tie-breaker pa pb na nb]
    [(op/op-type (first na))
     (op/op-type (first nb))]))

(defmulti compose-roots
  (fn [tie-breaker a b]
    [(op/op-type a)
     (op/op-type b)]))

(defn compose-seqs [ops-a ops-b]
  (loop [r []
         na ops-a
         nb ops-b]
    (if (or (seq na) (seq nb))
      (let [[ops na' nb'] (compose-in-seq na nb)]
        (recur (into r ops) na' nb'))
      r)))

(defn compose-maps [map-a map-b]
  (into map-a
        (map (fn [[k op-b]]
               [k (if-some [op-a (get map-a k)]
                    (compose-roots op-a op-b)
                    op-b)]))
        map-b))

(defn compose-subtrees [a b]
  (cond
    (and (sequential? a)
         (sequential? b))
    (compose-seqs a b)

    (and (map? a)
         (map? b))
    (compose-maps a b)

    :else (panic "mismatched subtrees encountered (one seq, one map)")))

(defn compose
  ([] '(retain))
  ([delta-a delta-b]
   (compose-roots delta-a delta-b)))

(defn split-next [ops off]
  (let [[split-1 split-2] (op/split (first ops) off)]
    (->> (next ops)
         (cons split-2)
         (cons split-1))))

(defn compose_retain-subtree_delete-one [a b]
  (list (first b)
        (materialize (second (op/values b))
                     (invert a))))

(defn compose_retain-subtree_replace [a b]
  (list (first b)
        (first (op/values b))
        (materialize (second (op/values b))
                     (invert a))))

(defn compose_replace_delete-one [a b]
  (list (first b)
        (first (op/values a))))

(defn compose_replace_replace [a b]
  (list (first b)
        (first (op/values a))
        (second (op/values b))))

;; insert

(defmethod compose-in-seq [:insert :insert] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [:insert :delete] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-values-length a b)
      ;; split the insert op
      :larger
      [[]
       (->> (next na)
            (cons (second (op/split a (op/length b)))))
       (next nb)]

      ;; split the delete op
      :smaller
      [[]
       (next na)
       (->> (next nb)
            (cons (second (op/split b (op/length a)))))]

      ;; exact overlap
      :else
      [[] (next na) (next nb)])))

(defmethod compose-in-seq [:insert :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:insert :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length a b)
      ;; split the insert op
      :larger
      (let [[split-1 split-2] (op/split a (op/length b))]
        [[split-1]
         (->> (next na)
              (cons split-2))
         (next nb)])

      ;; split the retain-range op
      :smaller
      [[a]
       (next na)
       (->> (next nb)
            (cons (second (op/split b (op/length a)))))]

      ;; exact overlap
      :equal
      [[a] (next na) (next nb)])))

(defmethod compose-in-seq [:insert :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length a 1)
      ;; ignore zero-length inserts
      :smaller
      [[] (next na) nb]

      ;; exact overlap
      :equal
      [[(list (first a) (materialize (first (op/values a)) b))]
       (next na)
       (next nb)]

      ;; split the insert
      :larger
      [[] (split-next na 1) nb])))

(defmethod compose-in-seq [:insert :replace] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length a 1)
      ;; ignore the insert
      :smaller
      [[] (next na) nb]

      ;; replace the value to be inserted
      :equal
      [[(list (first b) (second (op/values b)))]
       (next na)
       (next nb)]

      ;; split the insert and retry
      :larger
      [[] (split-next na 1) nb])))

(defmethod compose-in-seq [:insert nil] [na nb]
  ;; compose the insert
  [[(first na)] (next na) nb])

;; retain

(defmethod compose-in-seq [:retain :insert] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

(defmethod compose-in-seq [:retain :retain] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:retain :retain-range] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

(defmethod compose-in-seq [:retain :retain-subtree] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

(defmethod compose-in-seq [:retain :delete] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

(defmethod compose-in-seq [:retain :replace] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

(defmethod compose-in-seq [:retain nil] [na nb]
  (compose-in-seq (op/unwrap-retain-in-seq na) nb))

;; retain-range

(defmethod compose-in-seq [:retain-range :insert] [na nb]
  ;; compose the insert, retry the retain
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [:retain-range :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:retain-range :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length a b)
      ;; compose a, split b
      :smaller
      [[a]
       (next na)
       (->> (next nb)
            (cons (second (op/split b (op/length a)))))]

      ;; compose b, split a
      :larger
      [[b]
       (->> (next na)
            (cons (second (op/split a (op/length b)))))
       (next nb)]

      ;; exact overlap
      :equal
      [[a] (next na) (next nb)])))

(defmethod compose-in-seq [:retain-range :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length a 1)
      ;; ignore zero-length retains
      :smaller
      [[] (next na) nb]

      ;; compose the retain-subtree op
      :equal
      [[b] (next na) (next nb)]

      ;; split the retain-range op and retry
      :larger
      [[] (split-next na 1) nb])))

(defmethod compose-in-seq [:retain-range :delete] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-left-numeric-length a b)
      ;; split the delete op
      :smaller
      [[] na (split-next nb (op/length a))]

      ;; split the retain-length op
      :larger
      [[] (split-next na (op/length b)) nb]

      ;; exact overlap
      :equal
      [[b] (next na) (next nb)])))

(defmethod compose-in-seq [:retain-range :replace] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length a 1)
      ;; ignore zero-length retains
      :smaller
      [[] (next na) nb]

      ;; compose the replace op
      :equal
      [[b] (next na) (next nb)]

      ;; split the retain-range and retry
      :larger
      [[] (split-next na 1) nb])))

(defmethod compose-in-seq [:retain-range nil] [na nb]
  [[(first na)] (next na) nb])

;; retain-subtree

(defmethod compose-in-seq [:retain-subtree :insert] [na nb]
  ;; compose the insert op
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [:retain-subtree :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:retain-subtree :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length b 1)
      ;; ignore zero-length retains
      :smaller
      [[] na (next nb)]

      ;; compose retain-subtree
      :equal
      [[a] (next na) (next nb)]

      ;; split retain-range and retry
      :larger
      [[] na (split-next nb 1)])))

(defmethod compose-in-seq [:retain-subtree :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)]
    [[(compose-subtrees a b)]
     (next na)
     (next nb)]))

(defmethod compose-in-seq [:retain-subtree :delete] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length b 1)
      ;; ignore zero-length deletes
      :smaller
      [[] na (next nb)]

      ;; delete the subtree
      :equal
      [[(compose_retain-subtree_delete-one a b)]
       (next na)
       (next nb)]

      ;; split the delete and retry
      :larger
      [[] na (split-next nb 1)])))

(defmethod compose-in-seq [:retain-subtree :replace] [na nb]
  ;; replace the subtree
  (let [a (first na)
        b (first nb)]
    [[(compose_retain-subtree_replace a b)]
     (next na)
     (next nb)]))

(defmethod compose-in-seq [:retain-subtree nil] [na nb]
  [[(first na)] (next na) nb])

;; delete

(defmethod compose-in-seq [:delete :insert] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq [:delete :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:delete :retain-range] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq [:delete :retain-subtree] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq [:delete :delete] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq [:delete :replace] [na nb]
  [[(first na)] (next na) nb])

(defmethod compose-in-seq [:delete nil] [na nb]
  [[(first na)] (next na) nb])

;; replace

(defmethod compose-in-seq [:replace :insert] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [:replace :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [:replace :retain-range] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length b 1)
      ;; ignore zero-length retains
      :smaller
      [[] na (next nb)]

      ;; compose the replace op
      :equal
      [[a] (next na) (next nb)]

      ;; split the retain-range op and retry
      :larger
      [[] na (split-next nb 1)])))

(defmethod compose-in-seq [:replace :retain-subtree] [na nb]
  (let [a (first na)
        b (first nb)
        values (op/values a)]
    [[(list (first a) (first values) (materialize (second values) b))]
     (next na)
     (next nb)]))

(defmethod compose-in-seq [:replace :delete] [na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length b 1)
      ;; ignore zero-length deletes
      :smaller
      [[] na (next nb)]

      ;; compose the delete op
      :equal
      [[(compose_replace_delete-one a b)]
       (next na)
       (next nb)]

      ;; split delete op and retry
      :larger
      [[] na (split-next nb 1)])))

(defmethod compose-in-seq [:replace :replace] [na nb]
  (let [a (first na)
        b (first nb)]
    ;; the second replace replaces the first
    [[(compose_replace_replace a b)]
     (next na)
     (next nb)]))

(defmethod compose-in-seq [:replace nil] [na nb]
  [[(first na)] (next na) nb])

;; end-of-sequence

(defmethod compose-in-seq [nil :insert] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [nil :retain] [na nb]
  (compose-in-seq na (op/unwrap-retain-in-seq nb)))

(defmethod compose-in-seq [nil :retain-range] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [nil :retain-subtree] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [nil :delete] [na nb]
  [[(first nb)] na (next nb)])

(defmethod compose-in-seq [nil :replace] [na nb]
  [[(first nb)] na (next nb)])

;; insert

(defmethod compose-roots [:insert :insert] [a b]
  (panic "insert must not be used to replace existing values, use replace instead"))

(defmethod compose-roots [:insert :retain] [a b]
  (compose-roots a (op/unwrap-retain b)))

(defmethod compose-roots [:insert :retain-range] [a b]
  a)

(defmethod compose-roots [:insert :retain-subtree] [a b]
  (list (first a) (materialize (first (op/values a)) b)))

(defmethod compose-roots [:insert :delete] [a b]
  ;; this is a retain of a non-existing value, which is effectively a no-op
  '(retain))

(defmethod compose-roots [:insert :replace] [a b]
  (list (first a) (second (op/values b))))

;; retain

(defmethod compose-roots [:retain :insert] [a b]
  (compose-roots (op/unwrap-retain a) b))

(defmethod compose-roots [:retain :retain] [a b]
  (compose-roots (op/unwrap-retain a) (op/unwrap-retain b)))

(defmethod compose-roots [:retain :retain-range] [a b]
  (compose-roots (op/unwrap-retain a) b))

(defmethod compose-roots [:retain :retain-subtree] [a b]
  (compose-roots (op/unwrap-retain a) b))

(defmethod compose-roots [:retain :delete] [a b]
  (compose-roots (op/unwrap-retain a) b))

(defmethod compose-roots [:retain :replace] [a b]
  (compose-roots (op/unwrap-retain a) b))

;; retain-range

;; we allow retain-range as a no-op on non-existing map entries and nil root nodes.
(defmethod compose-roots [:retain-range :insert] [a b]
  b)

(defmethod compose-roots [:retain-range :retain] [a b]
  (compose-roots a (op/unwrap-retain b)))

(defmethod compose-roots [:retain-range :retain-range] [a b]
  b)

(defmethod compose-roots [:retain-range :retain-subtree] [a b]
  b)

(defmethod compose-roots [:retain-range :delete] [a b]
  b)

(defmethod compose-roots [:retain-range :replace] [a b]
  b)

;; retain-subtree

;; invalid (insert over existing | retain non-existing)
(defmethod compose-roots [:retain-subtree :insert] [a b]
  (panic "insert must not be used to replace existing values, use replace instead"))

(defmethod compose-roots [:retain-subtree :retain] [a b]
  (compose-roots a (op/unwrap-retain b)))

(defmethod compose-roots [:retain-subtree :retain-range] [a b]
  a)

(defmethod compose-roots [:retain-subtree :retain-subtree] [a b]
  (compose-subtrees a b))

(defmethod compose-roots [:retain-subtree :delete] [a b]
  (compose_retain-subtree_delete-one a b))

(defmethod compose-roots [:retain-subtree :replace] [a b]
  (compose_retain-subtree_replace a b))

;; delete

(defmethod compose-roots [:delete :insert] [a b]
  (list 'replace (first (op/values a)) (first (op/values b))))

(defmethod compose-roots [:delete :retain] [a b]
  (compose-roots a (op/unwrap-retain b)))

;; we allow retain-range as a no-op on non-existing map entries and nil root nodes.
(defmethod compose-roots [:delete :retain-range] [a b]
  a)

;; invalid (retain non-existing)
(defmethod compose-roots [:delete :retain-subtree] [a b]
  (panic "retain-subtree on a non-existing map entry or root node"))

;; invalid (delete non-existing)
(defmethod compose-roots [:delete :delete] [a b]
  (panic "delete on a non-existing map entry or root node"))

;; invalid (replace non-existing)
(defmethod compose-roots [:delete :replace] [a b]
  (panic "replace on a non-existing map entry or root node"))

;; replace

;; invalid (insert over existing | replace non-existing)
(defmethod compose-roots [:replace :insert] [a b]
  (panic "insert on a pre-existing map entry or root node"))

(defmethod compose-roots [:replace :retain] [a b]
  (compose-roots a (op/unwrap-retain b)))

(defmethod compose-roots [:replace :retain-range] [a b]
  a)

(defmethod compose-roots [:replace :retain-subtree] [a b]
  (list (first a)
        (first (op/values a))
        (materialize (second (op/values a)) b)))

(defmethod compose-roots [:replace :delete] [a b]
  (compose_replace_delete-one a b))

(defmethod compose-roots [:replace :replace] [a b]
  (compose_replace_replace a b))

