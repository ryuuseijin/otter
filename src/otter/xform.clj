(ns otter.xform
  (:require [otter.operations :as op]
            [otter.utils :refer :all]))

(defmulti xform-in-seq_any_any
  (fn [context pa pb na nb]
    [(:type (first na))
     (:type (first nb))]))

(defmulti xform-in-map_any_any
  (fn [context a b]
    [(:type a)
     (:type b)]))

(defn make-tie-breaker [revision-a revision-b]
  (let [id-breaker (compare (-> revision-a :id :process-id)
                            (-> revision-b :id :process-id))]
    (if-not (zero? id-breaker)
      id-breaker
      (let [time-breaker (compare (:time revision-a)
                                  (:time revision-b))]
        (if-not (zero? time-breaker)
          time-breaker
          (panic "unable to tie-break operations with identical process-id and time"))))))

(defn make-lww-tie-breaker [revision-a revision-b]
  (let [time-breaker (compare (:time revision-a)
                              (:time revision-b))]
    (if-not (zero? time-breaker)
      time-breaker
      (make-tie-breaker revision-a revision-b))))

(defn insert-to-retain [insert]
  (op/retain-range (op/op-length insert)))

(defn reverse-xform [xform-fn]
  (fn [context pa pb na nb]
    (let [[pb' pa' nb' na'] (xform-fn context pb pa nb na)]
      [pa' pb' na' nb'])))

(defn xform-seqs [context ops-a ops-b]
  (loop [pa []
         pb []
         na ops-a
         nb ops-b]
    (if (or (seq na) (seq nb)) ;;XX probably not necessary because we use next
      (let [[pa' pb' na' nb'] (xform-in-seq_any_any context pa pb na nb)]
        (recur pa' pb' na' nb'))
      [pa pb])))

(defn xform-ops [context op-a op-b]
  (when-not (and (= 1 (op/op-length op-a)
                    1 (op/op-length op-b)))
    (panic "operations on tree root and map values must have a length of exactly 1"))
  (xform-in-map_any_any context op-a op-b))

(defn xform-maps [context map-a map-b]
  (let [[k-op-a k-op-b]
        (->> (select-keys map-a (keys map-b))
             (map (fn [[k op-a]]
                    ;;XX this will introduce no-op retains which could
                    ;;   be filtered out immediately
                    (let [[op-a' op-b'] (xform-ops context op-a (get map-b k))]
                      [[k op-a']
                       [k op-b']])))
             (unzip))]
    [(into map-a k-op-a)
     (into map-b k-op-b)]))

(defn xform-subtrees [context subtree-a subtree-b]
  (cond
    (and (sequential? subtree-a)
         (sequential? subtree-b))
    (xform-seqs context subtree-a subtree-b)

    (and (map? subtree-a)
         (map? subtree-b))
    (xform-maps context subtree-a subtree-b)

    :else (panic "unable to xform mismatched subtree types")))

(defn xform [context delta-a delta-b]
  (let [[op-a op-b] (xform-ops context (:root-op delta-a) (:root-op delta-b))]
    [(assoc delta-a :root-op op-a)
     (assoc delta-b :root-op op-b)]))

(defn xform-context [& {:keys [tie-breaker lww-tie-breaker] :as context}]
  context)

(defn xform-revisions [revision-a revision-b]
  (let [[delta-a delta-b]
        (xform (xform-context
                :tie-breaker (make-tie-breaker revision-a revision-b)
                :lww-tie-breaker (make-lww-tie-breaker revision-a revision-b))
               (:delta revision-a)
               (:delta revision-b))]
    [(assoc revision-a :delta delta-a)
     (assoc revision-b :delta delta-b)]))

;; -------- xform-in-seq

(defn xform_insert_any [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    [(conj pa a)
     (conj pb (insert-to-retain a))
     (next na)
     nb]))

(def xform_any_insert
  (reverse-xform xform_insert_any))

(defn xform_insert_insert [context pa pb na nb]
  (if (pos? (:tie-breaker context))
    (xform_insert_any context pa pb na nb)
    (xform_any_insert context pa pb na nb)))

(defn xform_retain-range_retain-range [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= (:retain-length a)
         (:retain-length b))
      [(conj pa a)
       (conj pb b)
       (next na)
       (next nb)]

      (< (:retain-length a)
         (:retain-length b))
      (let [[split-1 split-2] (op/split-op b (:retain-length a))]
        [(conj pa a)
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      (> (:retain-length a)
         (:retain-length b))

      (let [[split-1 split-2] (op/split-op a (:retain-length b))]
        [(conj pa split-1)
         (conj pb b)
         (->> (next na)
              (cons split-2))
         (next nb)])

      :else (unreachable))))

(defn xform_delete-range_delete-range [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= (:delete-length a)
         (:delete-length b))
      [pa
       pb
       (next na)
       (next nb)]

      (< (:delete-length a)
         (:delete-length b))
      [pa
       pb
       (next na)
       (->> (next nb)
            (cons (second (op/split-op b (:delete-length a)))))]

      (> (:delete-length a)
         (:delete-length b))
      [pa
       pb
       (->> (next na)
            (cons (second (op/split-op a (:delete-length b)))))
       (next nb)]

      :else (unreachable))))


(defn xform_retain-range_delete-range [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= (:retain-length a)
         (:delete-length b))
      [pa
       (conj pb b)
       (next na)
       (next nb)]

      (< (:retain-length a)
         (:delete-length b))
      (let [[split-1 split-2] (op/split-op b (:retain-length a))]
        [pa
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      (> (:retain-length a)
         (:delete-length b))
      [pa
       (conj pb b)
       (->> (next na)
            (cons (second (op/split-op a (:delete-length b)))))
       (next nb)]

      :else (unreachable))))

(def xform_delete-range_retain-range
  (reverse-xform xform_retain-range_delete-range))

(defn xform_retain-range_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (:retain-length a)
      0 [pa pb (next na) nb]
      1 [(conj pa a)
         (conj pb b)
         (next na)
         (next nb)]
      [pa
       pb
       (let [[split-1 split-2] (op/split-op a 1)]
         (->> (next na)
              (cons split-2)
              (cons split-1)))
       nb])))

(def xform_retain-one_retain-range
  (reverse-xform xform_retain-range_retain-one))

(defn xform_delete-range_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (:delete-length a)
      0 [pa pb (next na) nb]
      1 [(conj pa a)
         pb
         (next na)
         (next nb)]
      [pa
       pb
       (let [[split-1 split-2] (op/split-op a 1)]
         (->> (next na)
              (cons split-2)
              (cons split-1)))
       nb])))

(def xform_retain-one_delete-range
  (reverse-xform xform_delete-range_retain-one))

(defn xform_retain-subtree_retain-subtree [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (let [[a-subtree b-subtree] (xform-subtrees context (:subtree a) (:subtree b))]
      [(conj pa (assoc a :subtree a-subtree))
       (conj pb (assoc b :subtree b-subtree))
       (next na)
       (next nb)])))

(defn xform_replace-value_replace-value [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (if (pos? (:lww-tie-breaker context))
      [(conj pa (insert-to-retain b))
       (conj pb b)
       (next na)
       (next nb)]
      [(conj pa a)
       (conj pb (insert-to-retain a))
       (next na)
       (next nb)])))

(defn xform_retain-subtree_replace-value [context pa pb na nb]
  (let [b (first nb)]
    [(conj pa (op/retain-range 1))
     (conj pb b)
     (next na)
     (next nb)]))

(def xform_replace-value_retain-subtree
  (reverse-xform xform_retain-subtree_replace-value))


(defn xform_mark_any [context pa pb na nb]
  (let [a (first na)]
    [(conj pa a)
     pb
     (next na)
     nb]))

(def xform_any_mark
  (reverse-xform xform_mark_any))

;; insert-values

(defmethod xform-in-seq_any_any [:insert-values :insert-values] [context pa pb na nb]
  (xform_insert_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values :retain-subtree] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values :delete-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values :replace-value] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

(defmethod xform-in-seq_any_any [:insert-values nil] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

;; retain-range

(defmethod xform-in-seq_any_any [:retain-range :insert-values] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range :retain-range] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range :retain-subtree] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range :delete-range] [context pa pb na nb]
  (xform_retain-range_delete-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range :replace-value] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-range nil] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na (cons (first na) nb)))

;; retain-subtree

(defmethod xform-in-seq_any_any [:retain-subtree :insert-values] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree :retain-range] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree :retain-subtree] [context pa pb na nb]
  (xform_retain-subtree_retain-subtree context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree :delete-range] [context pa pb na nb]
  (xform_retain-one_delete-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree :replace-value] [context pa pb na nb]
  (xform_retain-subtree_replace-value context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

(defmethod xform-in-seq_any_any [:retain-subtree nil] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na (cons op/retain nb)))

;; delete-range

(defmethod xform-in-seq_any_any [:delete-range :insert-values] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range :retain-range] [context pa pb na nb]
  (xform_delete-range_retain-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range :retain-subtree] [context pa pb na nb]
  (xform_delete-range_retain-one context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range :delete-range] [context pa pb na nb]
  (xform_delete-range_delete-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range :replace-value] [context pa pb na nb]
  (xform_delete-range_retain-one context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

(defmethod xform-in-seq_any_any [:delete-range nil] [context pa pb na nb]
  (xform_delete-range_retain-range
   context pa pb na
   (cons (op/retain-range (:delete-length (first na)))
         nb)))

;; replace-value

(defmethod xform-in-seq_any_any [:replace-value :insert-values] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value :retain-range] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value :retain-subtree] [context pa pb na nb]
  (xform_replace-value_retain-subtree context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value :delete-range] [context pa pb na nb]
  (xform_retain-one_delete-range context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value :replace-value] [context pa pb na nb]
  (xform_replace-value_replace-value context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

(defmethod xform-in-seq_any_any [:replace-value nil] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na (cons op/retain nb)))

;; mark

(defmethod xform-in-seq_any_any [:mark :insert-values] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark :retain-range] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark :retain-subtree] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark :delete-range] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark :replace-value] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark :mark] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

(defmethod xform-in-seq_any_any [:mark nil] [context pa pb na nb]
  (xform_mark_any context pa pb na nb))

;; end-of-sequence

(defmethod xform-in-seq_any_any [nil :insert-values] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq_any_any [nil :retain-range] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb (cons (first nb) na) nb))

(defmethod xform-in-seq_any_any [nil :retain-subtree] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb (cons op/retain na) nb))

(defmethod xform-in-seq_any_any [nil :delete-range] [context pa pb na nb]
  (xform_retain-range_delete-range
   context pa pb
   (cons (op/retain-range (:delete-length (first nb)))
         na)
   nb))

(defmethod xform-in-seq_any_any [nil :replace-value] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb (cons op/retain na) nb))

(defmethod xform-in-seq_any_any [nil :mark] [context pa pb na nb]
  (xform_any_mark context pa pb na nb))

;; -------- xform-in-map

(defn xform-in-map_insert_insert [context a b]
  (if (pos? (:lww-tie-breaker context))
    [op/retain b]
    [a op/retain]))

;; insert-values

;;XX this may introduce operations for which :have-replaced-value? is
;;true even though the tree was supposed to be clear of this undo data
(defmethod xform-in-map_any_any [:insert-values :insert-values] [context a b]
  (xform-in-map_insert_insert context
                              (op/replace-value (first (:values a))
                                                (first (:values b)))
                              (op/replace-value (first (:values b))
                                                (first (:values a)))))

;; This is valid becase we allow retain-range as a no-op on non-existing
;; map entries and nil root nodes.
(defmethod xform-in-map_any_any [:insert-values :retain-range] [context a b]
  [a op/retain])

;; invalid (insert over existing | retain non-existing)
(defmethod xform-in-map_any_any [:insert-values :retain-subtree] [context a b]
  (panic "insert over existing or retain non-existing"))

;; invalid (insert over existing | delete non-existing)
(defmethod xform-in-map_any_any [:insert-values :delete-range] [context a b]
  (panic "insert over existing or delete non-existing"))

;; invalid (insert over existing | replace non-existing)
(defmethod xform-in-map_any_any [:insert-values :replace-value] [context a b]
  (panic "insert over existing or replace non-existing"))

;; retain-range

;; This is valid becase we allow retain-range as a no-op on non-existing
;; map entries and nil root nodes.
(defmethod xform-in-map_any_any [:retain-range :insert-values] [context a b]
  [op/retain b])

(defmethod xform-in-map_any_any [:retain-range :retain-range] [context a b]
  [op/retain b])

(defmethod xform-in-map_any_any [:retain-range :retain-subtree] [context a b]
  [op/retain b])

(defmethod xform-in-map_any_any [:retain-range :delete-range] [context a b]
  [op/retain b])

(defmethod xform-in-map_any_any [:retain-range :replace-value] [context a b]
  [op/retain b])

;; retain-subtree

;; invalid (insert over existing | retain non-existing)
(defmethod xform-in-map_any_any [:retain-subtree :insert-values] [context a b]
  (panic "insert over existing or retain non-existing"))

(defmethod xform-in-map_any_any [:retain-subtree :retain-range] [context a b]
  [a op/retain])

(defmethod xform-in-map_any_any [:retain-subtree :retain-subtree] [context a b]
  (let [[a-subtree b-subtree] (xform-subtrees context (:subtree a) (:subtree b))]
    [(assoc a :subtree a-subtree)
     (assoc b :subtree b-subtree)]))

(defmethod xform-in-map_any_any [:retain-subtree :delete-range] [context a b]
  [op/retain b])

(defmethod xform-in-map_any_any [:retain-subtree :replace-value] [context a b]
  [op/retain b])

;; delete-range

;; invalid (insert over existing | delete non-existing)
(defmethod xform-in-map_any_any [:delete-range :insert-values] [context a b]
  (panic "insert over existing or delete non-existing"))

(defmethod xform-in-map_any_any [:delete-range :retain-range] [context a b]
  [a op/retain])

(defmethod xform-in-map_any_any [:delete-range :retain-subtree] [context a b]
  [a op/retain])

(defmethod xform-in-map_any_any [:delete-range :delete-range] [context a b]
  [op/retain op/retain])

(defmethod xform-in-map_any_any [:delete-range :replace-value] [context a b]
  [op/retain (op/insert-values [(:value b)])])

;; replace-value

;; invalid (insert over existing | replace non-existing)
(defmethod xform-in-map_any_any [:replace-value :insert-values] [context a b]
  (panic "insert over existing or replace non-existing"))

(defmethod xform-in-map_any_any [:replace-value :retain-range] [context a b]
  [a op/retain])

(defmethod xform-in-map_any_any [:replace-value :retain-subtree] [context a b]
  [a op/retain])

(defmethod xform-in-map_any_any [:replace-value :delete-range] [context a b]
  [(op/insert-values [(:value a)]) op/retain])

(defmethod xform-in-map_any_any [:replace-value :replace-value] [context a b]
  (xform-in-map_insert_insert context a b))
