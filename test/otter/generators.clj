(ns otter.generators
  (:require [clojure.test.check.generators :as gen]
            [clojure.zip :as z]
            [otter.core :as ot]
            [otter.operations :as op]
            [otter.compose :refer [compose]]
            [otter.materialize :refer [materialize]]
            [otter.utils :refer :all]))

(def elements-to-insert [[] {} :val1 :val2])

(def node-gen
  (gen/elements elements-to-insert))

(defn tree-gen [node-gen]
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/vector inner)
                  (gen/map gen/string-alphanumeric inner)
                  inner]))
   node-gen))

(defn operation-gen [node-gen node root?]
  ;; Any value may be retained, deleted or replaced, except a nil root
  ;; node (which represents a non-existing tree and therefore can't be
  ;; operated on).
  (let [gens (-> [(gen/return (op/retain-range 1))]
                 (cond-> (not (and root? (nil? node)))
                   (conj (gen/return (op/delete-range 1))
                         (gen/fmap #(op/replace-value %1) node-gen))))]
    (gen/no-shrink
     (gen/one-of
      (cond
        ;; Maps may have new keys inserted
        (map? node)
        (conj gens
              (->> (gen/tuple node-gen
                              (gen/such-that #(not (contains? node %))
                                             gen/string-alphanumeric))
                   (gen/fmap
                    (fn [[node new-key]]
                      (op/retain-subtree
                       {new-key (op/insert-values [node])})))))

        (sequential? node)
        ;; Sequences may have new values inserted anywhere in the
        ;; sequence including the end
        (conj gens
              (->> (gen/tuple (gen/frequency [[4 (gen/fmap #(op/insert-values [%])
                                                           node-gen)]
                                              [1 (gen/return (op/mark "id"))]])
                              (gen/choose 0 (count node)))
                   (gen/fmap
                    (fn [[insert-or-mark off]]
                      (op/retain-subtree
                       (-> []
                           (cond-> (pos? off) (conj (op/retain-range off)))
                           (conj insert-or-mark)))))))

        ;; A new value may be inserted to replace a nil root node
        (and root? (nil? node))
        (conj gens (gen/fmap #(op/insert-values [%]) node-gen))

        :else gens)))))

(defn nodes [tree]
  (tree-seq (some-fn sequential? map?) 
            #(cond-> % (map? %) vals)
            tree))

(defn make-op-path [tree pos op]
  (cond
    (zero? pos)
    [pos op]

    (map? tree)
    (reduce (fn [[pos _]  k]
              (let [[pos op-path] (make-op-path (get tree k) pos op)]
                (if op-path
                  (reduced [pos (op/retain-subtree {k op-path})])
                  [pos nil])))
            [(dec pos) nil]
            (keys tree))

    (sequential? tree)
    (reduce (fn [[pos _ off] v]
              (let [[pos op-path] (make-op-path v pos op)]
                (if op-path
                  (reduced [pos
                            (op/retain-subtree
                             (-> []
                                 (cond-> (pos? off) (conj (op/retain-range off)))
                                 (conj op-path)))])
                  [pos nil (inc off)])))
            [(dec pos) nil 0]
            tree)

    :else
    [(dec pos) nil]))

(defn make-delta [tree pos op]
  (let [[_ op-path] (make-op-path tree pos op)]
    (when-not op-path
      (panic "pos is larger than the number of nodes in tree"))
    (ot/delta op-path)))

(defn single-op-delta-gen [tree]
  (gen/let [pos (gen/choose 0 (dec (count (nodes tree))))
            op  (operation-gen node-gen ;;XX parameterize
                               (nth (nodes tree) pos)
                               (zero? pos))]
    (make-delta tree pos op)))

(defn single-op-deltas-gen [num-deltas tree]
  (if (pos? num-deltas)
    (gen/let [delta-1 (single-op-delta-gen tree)
              r (single-op-deltas-gen (dec num-deltas) (materialize tree delta-1))]
      (cons delta-1 r))
    (gen/return [])))

 ;;XX make it more likely to get deep deltas, seems like this makes a
 ;;   lot of ops on the root node. Probably because delete operations
 ;;   may delete entire subtrees of other operations.
(defn delta-gen
  ([num-ops tree] ;;XX reverse num-ops and tree
   (gen/let [deltas (single-op-deltas-gen num-ops tree)]
     (reduce compose deltas))))

(defn deltas-gen
  ([tree min-deltas max-deltas]
   (gen/let [num-deltas (gen/choose min-deltas max-deltas)]
     (deltas-gen tree num-deltas)))
  ([tree num-deltas]
   (if (pos? num-deltas)
     (gen/let [delta (delta-gen 10 tree) ;;XX parameterize
               deltas (deltas-gen (materialize tree delta) (dec num-deltas))]
       (cons delta deltas))
     (gen/return []))))
