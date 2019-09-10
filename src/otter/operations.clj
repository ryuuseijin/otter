(ns otter.operations)

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
(defn delete-range [delete-length]
  {:type :delete-range
   :delete-length delete-length})

;; - concurrent deletes eliminate replace-value ops in sequnces but not in maps
;; - can not be used to replace non-existing values in maps
;; - can not be used to replace a nil root node (empty tree)
(defn replace-value [value]
  {:type :replace-value
   :value value})

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
