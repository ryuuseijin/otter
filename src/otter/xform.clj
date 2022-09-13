(ns otter.xform
  (:require [otter.operations :as op]
            [otter.materialize :refer [materialize]]
            [otter.normalize :refer [normalize]]
            [otter.utils :refer :all]))

(defmulti xform-in-seq
  (fn [context pa pb na nb]
    [(op/op-type (first na))
     (op/op-type (first nb))]))

(defmulti xform-roots
  (fn [context a b]
    [(op/op-type a)
     (op/op-type b)]))

(defn make-tie-breaker [a-info b-info]
  (let [id-breaker (compare (:pid a-info)
                            (:pid b-info))]
    (if-not (zero? id-breaker)
      id-breaker
      (let [time-breaker (compare (:time a-info)
                                  (:time b-info))]
        (if-not (zero? time-breaker)
          time-breaker
          (panic "unable to tie-break operations with identical pid and time"))))))

(defn make-lww-tie-breaker [a-info b-info]
  (let [time-breaker (compare (:time a-info)
                              (:time b-info))]
    (if-not (zero? time-breaker)
      time-breaker
      (make-tie-breaker a-info b-info))))

(defn insert-or-replace-to-retain [insert-or-replace]
  (list 'retain (op/length insert-or-replace)))

(defn reverse-xform [xform-fn]
  (fn [context pa pb na nb]
    (let [[pb' pa' nb' na'] (xform-fn context pb pa nb na)]
      [pa' pb' na' nb'])))

(defn xform-seqs [context ops-a ops-b]
  (loop [pa []     ;; prev a
         pb []     ;; prev b
         na ops-a  ;; next a
         nb ops-b] ;; next b
    (if (or (seq na) (seq nb))
      (let [[pa' pb' na' nb'] (xform-in-seq context pa pb na nb)]
        (recur pa' pb' na' nb'))
      [pa pb])))

(defn xform-maps [context map-a map-b]
  (->> (select-keys map-a (keys map-b))
       (reduce (fn [[map-a map-b] [k op-a]]
                 (let [[op-a' op-b'] (xform-roots context op-a (get map-b k))]
                   [(assoc map-a k op-a')
                    (assoc map-b k op-b')]))
               [map-a map-b])))

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
  (let [[op-a op-b] (xform-roots {:tie-breaker (make-tie-breaker delta-a delta-b)
                                  :lww-tie-breaker (make-lww-tie-breaker delta-a delta-b)}
                                 (:root delta-a)
                                 (:root delta-b))]
    [(assoc delta-a :root (normalize op-a))
     (assoc delta-b :root (normalize op-b))]))

;; -------- xform-in-seq

(defn xform_insert_any [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    [(conj pa a)
     (conj pb (insert-or-replace-to-retain a))
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
    (case (op/compare-numeric-length a b)
      :equal
      [(conj pa a)
       (conj pb b)
       (next na)
       (next nb)]

      :smaller
      (let [[split-1 split-2] (op/split b (op/length a))]
        [(conj pa a)
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      :larger
      (let [[split-1 split-2] (op/split a (op/length b))]
        [(conj pa split-1)
         (conj pb b)
         (->> (next na)
              (cons split-2))
         (next nb)]))))

(defn xform_delete_delete [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-values-length a b)
      :equal
      [pa
       pb
       (next na)
       (next nb)]

      :smaller
      [pa
       pb
       (next na)
       (->> (next nb)
            (cons (second (op/split b (op/length a)))))]

      :larger
      [pa
       pb
       (->> (next na)
            (cons (second (op/split a (op/length b)))))
       (next nb)])))


(defn xform_retain-range_delete [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-left-numeric-length a b)
      :equal
      [pa
       (conj pb b)
       (next na)
       (next nb)]

      :smaller
      (let [[split-1 split-2] (op/split b (op/length a))]
        [pa
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      :larger
      [pa
       (conj pb b)
       (->> (next na)
            (cons (second (op/split a (op/length b)))))
       (next nb)])))

(def xform_delete_retain-range
  (reverse-xform xform_retain-range_delete))

(defn xform_retain-range_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-numeric-length a 1)
      :smaller
      [pa pb (next na) nb]

      :equal
      [(conj pa a)
       (conj pb b)
       (next na)
       (next nb)]

      :larger
      [pa
       pb
       (let [[split-1 split-2] (op/split a 1)]
         (->> (next na)
              (cons split-2)
              (cons split-1)))
       nb])))

(def xform_retain-one_retain-range
  (reverse-xform xform_retain-range_retain-one))

(defn xform_delete_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/compare-right-numeric-length a 1)
      :smaller
      [pa pb (next na) nb]

      :equal
      [(conj pa (list* (first a)
                       ;; b may be retain-subtree or replace
                       (materialize (first (op/values a)) b)
                       (rest (op/values a))))
       pb
       (next na)
       (next nb)]

      :larger
      [pa
       pb
       (let [[split-1 split-2] (op/split a 1)]
         (->> (next na)
              (cons split-2)
              (cons split-1)))
       nb])))


(def xform_delete_retain-subtree xform_delete_retain-one)
(def xform_retain-subtree_delete (reverse-xform xform_delete_retain-subtree))
(def xform_delete_replace  xform_delete_retain-one)
(def xform_replace_delete  (reverse-xform xform_delete_replace))

(defn xform_retain-subtree_retain-subtree [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (let [[a' b'] (xform-subtrees context a b)]
      [(conj pa a')
       (conj pb b')
       (next na)
       (next nb)])))

(defn xform_replace_replace [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (if (pos? (:lww-tie-breaker context))
      ;; b wins
      [(conj pa (insert-or-replace-to-retain b))
       (conj pb (list (first b)
                      (second  (op/values a))
                      (second (op/values b))))
       (next na)
       (next nb)]
      ;; a wins
      [(conj pa (list (first a)
                      (second  (op/values b))
                      (second (op/values a))))
       (conj pb (insert-or-replace-to-retain a))
       (next na)
       (next nb)])))

(defn xform_retain-subtree_replace [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    [(conj pa '(retain 1))
     (conj pb (list (first b)
                    (materialize (first (op/values b)) a)
                    (second (op/values b))))
     (next na)
     (next nb)]))

(def xform_replace_retain-subtree
  (reverse-xform xform_retain-subtree_replace))

;; insert

(defmethod xform-in-seq [:insert :insert] [context pa pb na nb]
  (xform_insert_insert context pa pb na nb))

(defmethod xform-in-seq [:insert :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:insert :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :retain-subtree] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :delete] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :replace] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert nil] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

;; retain


(defmethod xform-in-seq [:retain :insert] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

(defmethod xform-in-seq [:retain :retain] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:retain :retain-range] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

(defmethod xform-in-seq [:retain :retain-subtree] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

(defmethod xform-in-seq [:retain :delete] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

(defmethod xform-in-seq [:retain :replace] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

(defmethod xform-in-seq [:retain nil] [context pa pb na nb]
  (xform-in-seq context pa pb (op/unwrap-retain-in-seq na) nb))

;; retain-range

(defmethod xform-in-seq [:retain-range :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:retain-range :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:retain-range :retain-range] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na nb))

(defmethod xform-in-seq [:retain-range :retain-subtree] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb na nb))

(defmethod xform-in-seq [:retain-range :delete] [context pa pb na nb]
  (xform_retain-range_delete context pa pb na nb))

(defmethod xform-in-seq [:retain-range :replace] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb na nb))

(defmethod xform-in-seq [:retain-range nil] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na (cons (first na) nb)))

;; retain-subtree

(defmethod xform-in-seq [:retain-subtree :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:retain-subtree :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:retain-subtree :retain-range] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na nb))

(defmethod xform-in-seq [:retain-subtree :retain-subtree] [context pa pb na nb]
  (xform_retain-subtree_retain-subtree context pa pb na nb))

(defmethod xform-in-seq [:retain-subtree :delete] [context pa pb na nb]
  (xform_retain-subtree_delete context pa pb na nb))

(defmethod xform-in-seq [:retain-subtree :replace] [context pa pb na nb]
  (xform_retain-subtree_replace context pa pb na nb))

(defmethod xform-in-seq [:retain-subtree nil] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na (cons '(retain 1) nb)))

;; delete

(defmethod xform-in-seq [:delete :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:delete :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:delete :retain-range] [context pa pb na nb]
  (xform_delete_retain-range context pa pb na nb))

(defmethod xform-in-seq [:delete :retain-subtree] [context pa pb na nb]
  (xform_delete_retain-subtree context pa pb na nb))

(defmethod xform-in-seq [:delete :delete] [context pa pb na nb]
  (xform_delete_delete context pa pb na nb))

(defmethod xform-in-seq [:delete :replace] [context pa pb na nb]
  (xform_delete_replace context pa pb na nb))

(defmethod xform-in-seq [:delete nil] [context pa pb na nb]
  (xform_delete_retain-range context pa pb na
                             (cons (list 'retain (op/length (first na)))
                                   nb)))

;; replace

(defmethod xform-in-seq [:replace :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:replace :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [:replace :retain-range] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na nb))

(defmethod xform-in-seq [:replace :retain-subtree] [context pa pb na nb]
  (xform_replace_retain-subtree context pa pb na nb))

(defmethod xform-in-seq [:replace :delete] [context pa pb na nb]
  (xform_replace_delete context pa pb na nb))

(defmethod xform-in-seq [:replace :replace] [context pa pb na nb]
  (xform_replace_replace context pa pb na nb))

(defmethod xform-in-seq [:replace nil] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na (cons '(retain 1) nb)))

;; end-of-sequence

(defmethod xform-in-seq [nil :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [nil :retain] [context pa pb na nb]
  (xform-in-seq context pa pb na (op/unwrap-retain-in-seq nb)))

(defmethod xform-in-seq [nil :retain-range] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb (cons (first nb) na) nb))

(defmethod xform-in-seq [nil :retain-subtree] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb (cons '(retain 1) na) nb))

(defmethod xform-in-seq [nil :delete] [context pa pb na nb]
  (xform_retain-range_delete context pa pb
                             (cons (list 'retain (op/length (first nb)))
                                   na)
                             nb))

(defmethod xform-in-seq [nil :replace] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb (cons '(retain 1) na) nb))

;; -------- xform-roots

(defn reverse-xform-roots [xform-fn]
  (fn [context a b]
    (let [[b a] (xform-fn context b a)]
      [a b])))

;; preserve inserted roots of same collection type
(defn xform-roots_insert_insert [context a b]
  (let [a (first (op/values a))
        b (first (op/values b))]
    (cond
      (and (sequential? a)
           (sequential? b))
      (xform-seqs context (op/values a) (op/values b))
      
      (and (map? a)
           (map? b))
      (xform-maps context (op/values a) (op/values b))

      ;; XX instead one should win and the other should be reversed
      :else (panic "unable to xform mismatched subtree types"))))

(defn xform-roots_replace_replace [context a b]
  (if (pos? (:lww-tie-breaker context))
    ;; b wins
    ['(retain)
     (list (first b)
           (second (op/values a))
           (second (op/values b)))]
    ;; a wins
    [(list (first a)
           (second (op/values b))
           (second (op/values a)))
     '(retain)]))

(defn xform-roots_retain-subtree_replace [context a b]
  ['(retain)
   (list (first b)
         (materialize (first (op/values b)) a))])

(def xform-roots_replace_retain-subtree
  (reverse-xform-roots xform-roots_retain-subtree_replace))

(defn xform-roots_delete_retain-subtree [context a b]
  [(list (first a)
         (materialize (first (op/values a)) b))
   '(retain)])


(def xform-roots_retain-subtree_delete
  (reverse-xform-roots xform-roots_delete_retain-subtree))

(defn xform-roots_delete_replace [context a b]
  [(list (first a)
         (second (op/values b)))
   '(retain)])

(def xform-roots_replace_delete
  (reverse-xform-roots xform-roots_delete_replace))

;; insert

(defmethod xform-roots [:insert :insert] [context a b]
  (xform-roots_insert_insert context a b))

(defmethod xform-roots [:insert :retain] [context a b]
  [a (op/unwrap-retain b)])

;; This is valid because a retain acts as a no-op
(defmethod xform-roots [:insert :retain-range] [context a b]
  [a b])

;; invalid (insert over existing | retain non-existing)
(defmethod xform-roots [:insert :retain-subtree] [context a b]
  (panic "insert over existing or retain non-existing"))

;; invalid (insert over existing | delete non-existing)
(defmethod xform-roots [:insert :delete] [context a b]
  (panic "insert over existing or delete non-existing"))

;; invalid (insert over existing | replace non-existing)
(defmethod xform-roots [:insert :replace] [context a b]
  (panic "insert over existing or replace non-existing"))

;; retain

(defmethod xform-roots [:retain :insert] [context a b]
  [(op/unwrap-retain a) b])

(defmethod xform-roots [:retain :retain] [context a b]
  [(op/unwrap-retain a)
   (op/unwrap-retain b)])

(defmethod xform-roots [:retain :retain-range] [context a b]
  [(op/unwrap-retain a) b])

(defmethod xform-roots [:retain :retain-subtree] [context a b]
  [(op/unwrap-retain a) b])

(defmethod xform-roots [:retain :delete] [context a b]
  [(op/unwrap-retain a) b])

(defmethod xform-roots [:retain :replace] [context a b]
  [(op/unwrap-retain a) b])

;; retain-range

;; This is valid because a retain acts as a no-op
(defmethod xform-roots [:retain-range :insert] [context a b]
  [a b])

(defmethod xform-roots [:retain-range :retain] [context a b]
  [a (op/unwrap-retain b)])

(defmethod xform-roots [:retain-range :retain-range] [context a b]
  [a b])

(defmethod xform-roots [:retain-range :retain-subtree] [context a b]
  [a b])

(defmethod xform-roots [:retain-range :delete] [context a b]
  [a b])

(defmethod xform-roots [:retain-range :replace] [context a b]
  [a b])

;; retain-subtree

;; invalid (insert over existing | retain non-existing)
(defmethod xform-roots [:retain-subtree :insert] [context a b]
  (panic "insert over existing or retain non-existing"))

(defmethod xform-roots [:retain-subtree :retain] [context a b]
  [a (op/unwrap-retain b)])

(defmethod xform-roots [:retain-subtree :retain-range] [context a b]
  [a b])

(defmethod xform-roots [:retain-subtree :retain-subtree] [context a b]
  (xform-subtrees context a b))

(defmethod xform-roots [:retain-subtree :delete] [context a b]
  (xform-roots_retain-subtree_delete context a b))

(defmethod xform-roots [:retain-subtree :replace] [context a b]
  (xform-roots_retain-subtree_replace context a b))

;; delete

;; invalid (insert over existing | delete non-existing)
(defmethod xform-roots [:delete :insert] [context a b]
  (panic "insert over existing or delete non-existing"))

(defmethod xform-roots [:delete :retain] [context a b]
  [a (op/unwrap-retain b)])

(defmethod xform-roots [:delete :retain-range] [context a b]
  [a b])

(defmethod xform-roots [:delete :retain-subtree] [context a b]
  (xform-roots_delete_retain-subtree context a b))

(defmethod xform-roots [:delete :delete] [context a b]
  ['(retain) '(retain)])

(defmethod xform-roots [:delete :replace] [context a b]
  (xform-roots_delete_replace context a b))

;; replace

;; invalid (insert over existing | replace non-existing)
(defmethod xform-roots [:replace :insert] [context a b]
  (panic "insert over existing or replace non-existing"))

(defmethod xform-roots [:replace :retain] [context a b]
  [a (op/unwrap-retain b)])

(defmethod xform-roots [:replace :retain-range] [context a b]
  [a '(retain)])

(defmethod xform-roots [:replace :retain-subtree] [context a b]
  (xform-roots_replace_retain-subtree context a b))

(defmethod xform-roots [:replace :delete] [context a b]
  (xform-roots_replace_delete context a b))

(defmethod xform-roots [:replace :replace] [context a b]
  (xform-roots_replace_replace context a b))
