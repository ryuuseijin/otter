(ns otter.operations
  (:require [otter.utils :refer :all]))

;; - can not be used to replace existing values in maps
;; - can be used to replace a nil root node (empty tree)
(defn insert-values [values]
  {:type :insert-values
   :values values})

;; - can be omitted from the end of a sequence
;; - can not be used to retain non-existing items in a sequence
;; - but can be used to retain non-existing items in maps or root nodes
(defn retain-range [retain-length]
  {:type :retain-range
   :retain-length retain-length})

;; a concurrent delete deletes the entire subtree including all inserts
(defn retain-subtree [subtree]
  {:type :retain-subtree
   :subtree subtree})

;; deletes entire subtrees even if subtrees have concurrent inserts
(defn delete-range
  ([delete-length deleted-values]
   {:type :delete-range
    :delete-length delete-length
    :deleted-values deleted-values
    :have-deleted-values? true})
  ([delete-length]
   (assoc (delete-range delete-length [])
          :have-deleted-values? false)))

;; - concurrent deletes eliminate replace-value ops in sequences but not
;;   in maps because in sequences we would lose the identity of the
;;   resulting insert (xform replace and delete = insert) but with maps
;;   we don't (the map key is the identifier)
;; - can not be used to replace non-existing values in maps
;; - can not be used to replace a nil root node (empty tree)
(defn replace-value
  ([value replaced-value]
   {:type :replace-value
    :value value
    :replaced-value replaced-value
    :have-replaced-value? true})
  ([value]
   (assoc (replace-value value nil)
          :have-replaced-value? false)))

;; TODO: left-bias and right-bias flag
(defn mark [id]
  {:type :mark
   :id id})

(def retain (retain-range 1))

(defmulti op-length :type)

(defmethod op-length :insert-values [op]
  (count (:values op)))

(defmethod op-length :retain-range [op]
  (:retain-length op))

(defmethod op-length :retain-subtree [op]
  1)

(defmethod op-length :delete-range [op]
  (:delete-length op))

(defmethod op-length :replace-value [op]
  1)

(defmethod op-length :mark [op]
  0)

(defmulti split-op (fn [op off]
                     (:type op)))

(defmethod split-op :insert-values [op off]
  (when (< (count (take off (:values op)))
           off)
    (panic "splitting insert-values with split offset out of bounds"))
  [(update op :values #(take off %))
   (update op :values #(drop off %))])

(defmethod split-op :retain-range [op off]
  (when (< (:retain-length op)
           off)
    (panic "splitting retain-range with split offset out of bounds"))
  [(assoc op :retain-length off)
   (update op :retain-length - off)])

(defmethod split-op :delete-range [op off]
  (when (< (:delete-length op)
           off)
    (panic "splitting delete-range with split offset out of bounds"))
  [(-> op
       (assoc :delete-length off)
       (update :deleted-values #(take off %)))
   (-> op
       (update :delete-length - off)
       (update :deleted-values #(drop off %)))])

(defmethod split-op :retain-subtree [op off]
  (panic "retain-subtree can not be split"))

(defmethod split-op :replace-value [op off]
  (panic "replace-value can not be split"))

(defmethod split-op :mark [op off]
  (panic "mark can not be split"))

(defmulti join-ops (fn [a b]
                     (when (not= (:type a) (:type b))
                       (panic "incompatible types can not be joined"))
                     (:type a)))

(defmethod join-ops :insert-values [a b]
  (update a :values #(into (vec %) (:values b))))

(defmethod join-ops :retain-range [a b]
  (update a :retain-length + (:retain-length b)))

(defmethod join-ops :delete-range [a b]
  (-> a
      (update :delete-length + (:delete-length b))
      (update :deleted-values #(into (vec %) (:deleted-values b)))))

(defmethod join-ops :retain-subtree [a b]
  (panic "retain-subtree ops can not be joined"))

(defmethod join-ops :replace-value [a b]
  (panic "replace-value ops can not be joined"))

(defmethod join-ops :mark [a b]
  (panic "mark ops can not be joined"))
