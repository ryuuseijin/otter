(ns otter.xform
  (:require [otter.operations :as op]))

(defn tie-breaker [rev-a rev-b]
  (let [id-breaker (compare (:pid rev-a)
                            (:pid rev-b))]
    (if-not (zero? id-breaker)
      id-breaker
      (let [time-breaker (compare (:time rev-a)
                                  (:time rev-b))]
        (if-not (zero? time-breaker)
          time-breaker
          (panic "unable to tie-break operations with identical pid and time"))))))

(defn lww-tie-breaker [rev-a rev-b]
  (let [time-breaker (compare (:time rev-a)
                              (:time rev-b))]
    (if-not (zero? time-breaker)
      time-breaker
      (tie-breaker rev-a rev-b))))

(defmulti xform-in-seq
  (fn [tie-breaker pa pb na nb]
    [(op-type-in-seq (first na))
     (op-type-in-seq (first nb))]))

(defmulti xform-roots
  (fn [tie-breaker a b]
    [(op-type-as-root a)
     (op-type-as-root b)]))

(defn xform-seqs [tie-breaker ops-a ops-b]
  (loop [pa []     ;; prev a
         pb []     ;; prev b
         na ops-a  ;; next a
         nb ops-b] ;; next b
    (if (or (seq na) (seq nb))
      (let [[pa' pb' na' nb'] (xform-in-seq tie-breaker pa pb na nb)]
        (recur pa' pb' na' nb'))
      [pa pb])))

(defn xform-maps [tie-breaker map-a map-b]
  (->> (select-keys map-a (keys map-b))
       (reduce (fn [[map-a map-b] [k op-a]]
                 (let [[op-a' op-b'] (xform-roots tie-breaker op-a (get map-b k))]
                   [(assoc map-a k op-a')
                    (assoc map-b k op-b')]))
               [map-a map-b])))


(defn xform-delta [delta-a delta-b]
  (let [[op-a op-b] (xform-roots tie-breaker (:root delta-a) (:root delta-b))]
    [(assoc delta-a :root op-a)
     (assoc delta-b :root op-b)]))

(defn reverse-xform [xform-fn]
  (fn [context pa pb na nb]
    (let [[pb' pa' nb' na'] (xform-fn context pb pa nb na)]
      [pa' pb' na' nb'])))

(defn insert-to-retain [insert]
  (op/retain (op/length insert)))

;; -------- xform-in-seq

;;
(defn xform_insert_any [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    [(conj pa a)
     (conj pb (insert-to-retain a))
     (next na)
     nb]))

;;
(def xform_any_insert
  (reverse-xform xform_insert_any))

(defn xform_insert_insert [context pa pb na nb]
  (if (pos? (:tie-breaker context))
    (xform_insert_any context pa pb na nb)
    (xform_any_insert context pa pb na nb)))

;;
(defn xform_retain-range_retain-range [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= a b)
      [(conj pa a)
       (conj pb b)
       (next na)
       (next nb)]

      (< a b)
      (let [[split-1 split-2] (op/split b a)]
        [(conj pa a)
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      (> a b)
      (let [[split-1 split-2] (op/split a b)]
        [(conj pa split-1)
         (conj pb b)
         (->> (next na)
              (cons split-2))
         (next nb)])

      :else (unreachable))))

;;
(defn xform_delete_delete [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= (op/length a)
         (op/length b))
      [pa
       pb
       (next na)
       (next nb)]

      (< (op/length a)
         (op/length b))
      [pa
       pb
       (next na)
       (->> (next nb)
            (cons (second (op/split-op b (op/length a)))))]

      (> (op/length a)
         (op/length b))
      [pa
       pb
       (->> (next na)
            (cons (second (op/split-op a (op/length b)))))
       (next nb)]

      :else (unreachable))))


;;
(defn xform_retain-range_delete [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (cond
      (= a (op/length b))
      [pa
       (conj pb b)
       (next na)
       (next nb)]

      (< a (op/length b))
      (let [[split-1 split-2] (op/split-op b a)]
        [pa
         (conj pb split-1)
         (next na)
         (->> (next nb)
              (cons split-2))])

      (> a (op/length b))
      [pa
       (conj pb b)
       (->> (next na)
            (cons (second (op/split-op a (op/length b)))))
       (next nb)]

      :else (unreachable))))

;;
(def xform_delete_retain-range
  (reverse-xform xform_retain-range_delete-range))

;;
(defn xform_retain-range_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case a
      0 [pa pb (next na) nb]
      1 [(conj pa a)
         (conj pb b)
         (next na)
         (next nb)]
      [pa
       pb
       (->> (next na)
            (cons (- a 1))
            (cons 1))
       nb])))

;;
(def xform_retain-one_retain-range
  (reverse-xform xform_retain-range_retain-one))

;;
(defn xform_delete_retain-one [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (case (op/length a) ;;don't need length, just >1? XX
      0 [pa pb (next na) nb]
      1 [(conj pa (materialize (second a) b)) ;; b may be retain-subtree or replace-value XX
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

;;
(def xform_retain-one_delete
  (reverse-xform xform_delete_retain-one))

;;
(defn xform_retain-seq_retain-seq [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (let [[a' b'] (xform-seqs context a b)]
      [(conj pa a')
       (conj pb b')
       (next na)
       (next nb)])))

(defn xform_replace-value_replace-value [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    (if (pos? (:lww-tie-breaker context))
      [(conj pa (insert-to-retain b))
       (conj pb (cond-> b
                  (:have-replaced-value? b)
                  (assoc :replaced-value (:value a))))
       (next na)
       (next nb)]
      [(conj pa (cond-> a
                  (:have-replaced-value? a)
                  (assoc :replaced-value (:value b))))
       (conj pb (insert-to-retain a))
       (next na)
       (next nb)])))

(defn xform_retain-subtree_replace-value [context pa pb na nb]
  (let [a (first na)
        b (first nb)]
    [(conj pa (op/retain-range 1))
     (conj pb (cond-> b
                (:have-replaced-value? b)
                (update :replaced-value materialize a)))
     (next na)
     (next nb)]))

(def xform_replace-value_retain-subtree
  (reverse-xform xform_retain-subtree_replace-value))

;; insert

(defmethod xform-in-seq [:insert :insert] [context pa pb na nb]
  (xform_insert_insert context pa pb na nb))

(defmethod xform-in-seq [:insert :retain] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :delete] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :replace] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :retain-seq] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :retain-map] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:insert nil] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

;; retain

(defmethod xform-in-seq [:retain :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:retain :retain] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na nb))

(defmethod xform-in-seq [:retain :delete] [context pa pb na nb]
  (xform_retain-range_delete-range context pa pb na nb))

(defmethod xform-in-seq [:retain :replace] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb na nb))

(defmethod xform-in-seq [:retain :retain-seq] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:retain :retain-map] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:retain :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:retain nil] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb na (cons (first na) nb)))

;; delete

(defmethod xform-in-seq [:delete :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:delete :retain] [context pa pb na nb]
  (xform_delete-range_retain-range context pa pb na nb))

(defmethod xform-in-seq [:delete :delete] [context pa pb na nb]
  (xform_delete-range_delete-range context pa pb na nb))

(defmethod xform-in-seq [:delete :replace] [context pa pb na nb]
  (xform_delete-range_replace-value context pa pb na nb))

(defmethod xform-in-seq [:delete :retain-seq] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:delete :retain-map] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:delete :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:delete nil] [context pa pb na nb]
  (xform_delete-range_retain-range
   context pa pb na
   (cons (op/retain-range (:delete-length (first na)))
         nb)))

;; replace

(defmethod xform-in-seq [:replace :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [:replace :retain] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na nb))

(defmethod xform-in-seq [:replace :delete] [context pa pb na nb]
  (xform_replace-value_delete-range context pa pb na nb))

(defmethod xform-in-seq [:replace :replace] [context pa pb na nb]
  (xform_replace-value_replace-value context pa pb na nb))

(defmethod xform-in-seq [:replace :retain-seq] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:replace :retain-map] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:replace :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [:replace nil] [context pa pb na nb]
  (xform_retain-one_retain-range context pa pb na (cons op/retain nb)))

;; end-of-sequence

(defmethod xform-in-seq [nil :insert] [context pa pb na nb]
  (xform_any_insert context pa pb na nb))

(defmethod xform-in-seq [nil :retain-range] [context pa pb na nb]
  (xform_retain-range_retain-range context pa pb (cons (first nb) na) nb))

(defmethod xform-in-seq [nil :delete] [context pa pb na nb]
  (xform_retain-range_delete-range
   context pa pb
   (cons (op/retain-range (:delete-length (first nb)))
         na)
   nb))

(defmethod xform-in-seq [nil :replace] [context pa pb na nb]
  (xform_retain-range_retain-one context pa pb (cons op/retain na) nb))

(defmethod xform-in-seq [nil :retain-seq] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [nil :retain-map] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

(defmethod xform-in-seq [nil :retain-range] [context pa pb na nb]
  (xform_insert_any context pa pb na nb))

;; -------- xform-roots

(defn reverse-xform-roots [xform-fn]
  (fn [context a b]
    (let [[b a] (xform-fn context b a)]
      [a b])))

(defn xform-roots_insert_insert [context a b]
  (if (pos? (:lww-tie-breaker context))
    [op/retain b]
    [a op/retain]))

(defn xform-roots_replace_replace [context a b]
  (if (pos? (:lww-tie-breaker context))
    [op/retain
     (cond-> b
       (:have-replaced-value? b)
       (assoc :replaced-value (:value a)))]
    [(cond-> a
       (:have-replaced-value? a)
       (assoc :replaced-value (:value b)))
     op/retain]))

(defn xform-roots_retain_replace [context a b]
  [op/retain
   (cond-> b
     (:have-replaced-value? b)
     (update :replaced-value materialize a))])

(def xform-roots_replace_retain
  (reverse-xform-roots xform-roots_retain_replace))

(defn xform-roots_delete-range_retain [context a b]
  [(cond-> a
     (:have-deleted-values? a)
     (update :deleted-values #(-> %
                                  first
                                  (materialize b)
                                  vector)))
   op/retain])


(def xform-roots_retain_delete-range
  (reverse-xform-roots xform-roots_delete-range_retain))

;; insert

;;XX this may introduce operations for which :have-replaced-value? is
;;true even though the tree was supposed to be clear of this undo data
(defmethod xform-roots [:insert :insert] [context a b]
  (xform-roots_insert_insert context
                              (op/replace (first (:values a))
                                                (first (:values b)))
                              (op/replace (first (:values b))
                                                (first (:values a)))))

;; This is valid becase we allow retain-range as a no-op on non-existing
;; map entries and nil root nodes.
(defmethod xform-roots [:insert :retain-range] [context a b]
  [a op/retain])

;; invalid (insert over existing | retain non-existing)
(defmethod xform-roots [:insert :retain] [context a b]
  (panic "insert over existing or retain non-existing"))

;; invalid (insert over existing | delete non-existing)
(defmethod xform-roots [:insert :delete-range] [context a b]
  (panic "insert over existing or delete non-existing"))

;; invalid (insert over existing | replace non-existing)
(defmethod xform-roots [:insert :replace] [context a b]
  (panic "insert over existing or replace non-existing"))

;; retain

;; This is valid becase we allow retain-range as a no-op on non-existing
;; map entries and nil root nodes.
(defmethod xform-roots [:retain :insert] [context a b]
  [op/retain b])

(defmethod xform-roots [:retain :retain] [context a b]
  [op/retain b])

(defmethod xform-roots [:retain :retain] [context a b]
  [op/retain b])

(defmethod xform-roots [:retain :delete-range] [context a b]
  [op/retain b])

(defmethod xform-roots [:retain :replace] [context a b]
  [op/retain b])

;; delete

;; invalid (insert over existing | delete non-existing)
(defmethod xform-roots [:delete :insert] [context a b]
  (panic "insert over existing or delete non-existing"))

(defmethod xform-roots [:delete :retain] [context a b]
  [a op/retain])

(defmethod xform-roots [:delete :retain] [context a b]
  (xform-roots_delete_retain context a b))

(defmethod xform-roots [:delete :delete] [context a b]
  (assert (or (not (and (:have-deleted-values? a)
                        (:have-deleted-values? b)))
               (= (:deleted-values a)
                  (:deleted-values b))))
  [op/retain op/retain])

;;XX maybe a deleted value should not be replaced to make it equal to
;;the in-seq handling?
(defmethod xform-roots [:delete :replace] [context a b]
  [op/retain (op/insert [(:value b)])])

;; replace

;; invalid (insert over existing | replace non-existing)
(defmethod xform-roots [:replace :insert] [context a b]
  (panic "insert over existing or replace non-existing"))

(defmethod xform-roots [:replace :retain] [context a b]
  [a op/retain])

(defmethod xform-roots [:replace :retain] [context a b]
  (xform-roots_replace_retain context a b))

;;XX maybe a deleted value should not be replaced to make it equal to
;;the in-seq handling?
(defmethod xform-roots [:replace :delete] [context a b]
  [(op/insert [(:value a)]) op/retain])

(defmethod xform-roots [:replace :replace] [context a b]
  (xform-roots_replace_replace context a b))


;; insert

(defmethod xform-trees [:insert :insert] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :retain] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :delete] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :replace] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :retain-seq] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :retain-map] [context pa pb na nb]
  )

(defmethod xform-trees [:insert :retain-range] [context pa pb na nb]
  )

(defmethod xform-trees [:insert nil] [context pa pb na nb]
  )

;; retain

(defmethod xform-trees [:retain :insert] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :retain] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :delete] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :replace] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :retain-seq] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :retain-map] [context pa pb na nb]
  )

(defmethod xform-trees [:retain :retain-range] [context pa pb na nb]
  )

(defmethod xform-trees [:retain nil] [context pa pb na nb]
  )

;; delete

(defmethod xform-trees [:delete :insert] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :retain] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :delete] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :replace] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :retain-seq] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :retain-map] [context pa pb na nb]
  )

(defmethod xform-trees [:delete :retain-range] [context pa pb na nb]
  )

(defmethod xform-trees [:delete nil] [context pa pb na nb]
  )

;; replace

(defmethod xform-trees [:replace :insert] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :retain] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :delete] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :replace] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :retain-seq] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :retain-map] [context pa pb na nb]
  )

(defmethod xform-trees [:replace :retain-range] [context pa pb na nb]
  )

(defmethod xform-trees [:replace nil] [context pa pb na nb]
  )

;; end-of-sequence

(defmethod xform-trees [nil :insert] [context pa pb na nb]
  )

(defmethod xform-trees [nil :retain] [context pa pb na nb]
  )

(defmethod xform-trees [nil :delete] [context pa pb na nb]
  )

(defmethod xform-trees [nil :replace] [context pa pb na nb]
  )

(defmethod xform-trees [nil :retain-seq] [context pa pb na nb]
  )

(defmethod xform-trees [nil :retain-map] [context pa pb na nb]
  )

(defmethod xform-trees [nil :retain-range] [context pa pb na nb]
  )
