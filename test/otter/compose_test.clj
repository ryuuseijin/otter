(ns otter.compose-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [otter.core :as ot]
            [otter.operations :as op]
            [otter.compose :refer [compose compose-ops compose-seqs]]
            [otter.generators :as otter-gen]
            [otter.materialize :refer [materialize]]))

(def compose-same-materialize-result-prop
  (prop/for-all [[tree deltas]
                 (gen/let [tree (otter-gen/tree-gen otter-gen/node-gen)
                           num-ops (gen/choose 0 100)
                           deltas (otter-gen/single-op-deltas-gen num-ops tree)]
                   [tree deltas])]
     (is (= (reduce materialize tree deltas)
            (materialize tree (reduce compose deltas))))))

(deftest insert-values
  (testing "composing insert-values with"
    (testing "insert-values"
      (testing "on map values or root nodes"
        (is (= (op/insert-values [:val2])
               (compose-ops (op/insert-values [:val1])
                            (op/insert-values [:val2])))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val2])
                (op/insert-values [:val1])]
               (compose-seqs [(op/insert-values [:val1])]
                             [(op/insert-values [:val2])])))
        (testing "(found through generative testing)"
          (is (= [(op/insert-values [:val2])
                  (op/insert-values [:val1])]
                 (compose-seqs [(op/insert-values [:val1])]
                               [(op/insert-values [:val2])
                                (op/retain-range 1)]))))))
    (testing "retain-range"
      (testing "on maps or root nodes"
        (is (= (op/insert-values [:val1])
               (compose-ops (op/insert-values [:val1])
                            (op/retain-range 1)))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val1])]
               (compose-seqs [(op/insert-values [:val1])]
                             [(op/retain-range 1)]))))
      (testing "splits the insert-values if it is wider than the retain"
        (is (= [(op/insert-values [:val1])
                (op/insert-values [:val2])]
               (compose-seqs [(op/insert-values [:val1 :val2])]
                             [(op/retain-range 1)]))))
      (testing "splits the retain-range if it is wider than the insert"
        (is (= [(op/insert-values [:val])
                (op/retain-range 1)]
               (compose-seqs [(op/insert-values [:val])]
                             [(op/retain-range 2)])))))
    (testing "retain-subtree"
      (testing "on maps or root nodes"
        (is (= (op/insert-values [{:key :val}])
               (compose-ops (op/insert-values [{}])
                            (op/retain-subtree {:key (op/insert-values [:val])})))))
      (testing "on sequences"
        (is (= [(op/insert-values [{:key :val}])]
               (compose-seqs [(op/insert-values [{}])]
                             [(op/retain-subtree {:key (op/insert-values [:val])})]))))
      (testing "splits the insert if it is wider than 1"
        (is (= [(op/insert-values [{}])
                (op/insert-values [:val2])]
               (compose-seqs [(op/insert-values [{} :val2])]
                             [(op/retain-subtree {})])))))
    (testing "delete-range"
      (testing "on maps or root nodes"
        (is (= op/retain
               (compose-ops (op/insert-values [:val1])
                            (op/delete-range 1)))))
      (testing "on sequences"
        (is (= []
               (compose-seqs [(op/insert-values [:val1])]
                             [(op/delete-range 1)]))))
      (testing "splits the insert-values if it is wider than the retain-range"
        (is (= [(op/insert-values [:val2])]
               (compose-seqs [(op/insert-values [:val1 :val2])]
                             [(op/delete-range 1)]))))
      (testing "splits the delete-range if it is wider than the insert-values"
        (is (= [(op/delete-range 1)]
               (compose-seqs [(op/insert-values [:val])]
                             [(op/delete-range 2)])))))
    (testing "replace-value"
      (testing "on maps or root nodes"
        (is (= (op/insert-values [:val2])
               (compose-ops (op/insert-values [:val1])
                            (op/replace-value :val2)))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val2])]
               (compose-seqs [(op/insert-values [:val1])]
                             [(op/replace-value :val2)])))))
    (testing "mark"
      (is (= [(op/mark "id")
              (op/insert-values [:val])]
             (compose-seqs [(op/insert-values [:val])]
                           [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [(op/insert-values [:val])]
             (compose-seqs [(op/insert-values [:val])]
                           []))))))

(deftest retain-range
  (testing "composing retain-range with"
    (testing "insert-values"
      (testing "on map values or root nodes (error found through generative testing)"
        (is (= (op/insert-values [:val])
               (compose-ops (op/retain-range 1)
                            (op/insert-values [:val])))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val])
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/insert-values [:val])])))))
    (testing "retain-range"
      (testing "on map values or root nodes"
        (is (= (op/retain-range 1)
               (compose-ops (op/retain-range 1)
                            (op/retain-range 1)))))
      (testing "on sequences"
        (is (= [(op/retain-range 1)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/retain-range 1)]))))
      (testing "splits the first retain-range if it is wider than the other"
        (is (= [(op/retain-range 1)
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 2)]
                             [(op/retain-range 1)]))))
      (testing "splits the second retain-range if it is wider than the other"
        (is (= [(op/retain-range 1)
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/retain-range 2)])))))
    (testing "retain-subtree"
      (testing "on map values or root nodes"
        (is (= (op/retain-subtree {})
               (compose-ops (op/retain-range 1)
                            (op/retain-subtree {})))))
      (testing "on sequences"
        (is (= [(op/retain-subtree [])]
               (compose-seqs [(op/retain-range 1)]
                             [(op/retain-subtree [])]))))
      (testing "splits the retain-range if it is wider than 1"
        (is (= [(op/retain-subtree {})
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 2)]
                             [(op/retain-subtree {})])))))
    (testing "delete-range"
      (testing "on map values or root nodes"
        (is (= (op/delete-range 1)
               (compose-ops (op/retain-range 1)
                            (op/delete-range 1)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/delete-range 1)]))))
      (testing "splits the retain-range if it is wider than the delete-range"
        (is (= [(op/delete-range 1)
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 2)]
                             [(op/delete-range 1)]))))
      (testing "splits the delete-range if it is wider than the retain-range"
        (is (= [(op/delete-range 1)
                (op/delete-range 1)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/delete-range 2)])))))
    (testing "replace-value"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val)
               (compose-ops (op/retain-range 1)
                            (op/replace-value :val)))))
      (testing "on sequences"
        (is (= [(op/replace-value :val)]
               (compose-seqs [(op/retain-range 1)]
                             [(op/replace-value :val)]))))
      (testing "splits the retain-range if it is wider than 1"
        (is (= [(op/replace-value :val)
                (op/retain-range 1)]
               (compose-seqs [(op/retain-range 2)]
                             [(op/replace-value :val)])))))
    (testing "mark"
      (is (= [(op/mark "id") (op/retain-range 1)]
             (compose-seqs [(op/retain-range 1)]
                           [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [(op/retain-range 1)]
             (compose-seqs []
                           [(op/retain-range 1)]))))))

(deftest retain-subtree
  (testing "composing retain-subtree with"
    (testing "insert-values"
      (testing "on map values or root nodes"
        (is (= (op/insert-values [:val])
               (compose-ops (op/retain-subtree {})
                            (op/insert-values [:val])))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val])
                (op/retain-subtree {})]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/insert-values [:val])])))))
    (testing "retain-range"
      (testing "on map values or root nodes"
        (is (= (op/retain-subtree {})
               (compose-ops (op/retain-subtree {})
                            (op/retain-range 1)))))
      (testing "on sequences"
        (is (= [(op/retain-subtree {})]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/retain-range 1)]))))
      (testing "splits the retain-range if it is wider than 1"
        (is (= [(op/retain-subtree {})
                (op/retain-range 1)]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/retain-range 2)])))))
    (testing "retain-subtree"
      (testing "on map values or root nodes"
        (is (= (op/retain-subtree {:key1 (op/insert-values [:val1])
                                   :key2 (op/insert-values [:val2])})
               (compose-ops (op/retain-subtree {:key1 (op/insert-values [:val1])})
                            (op/retain-subtree {:key2 (op/insert-values [:val2])})))))
      (testing "on sequences"
        (is (= [(op/retain-subtree [(op/insert-values [:val2])
                                    (op/insert-values [:val1])])]
               (compose-seqs [(op/retain-subtree [(op/insert-values [:val1])])]
                             [(op/retain-subtree [(op/insert-values [:val2])])])))))

    (testing "delete-range"
      (testing "on map values or root nodes"
        (is (= (op/delete-range 1)
               (compose-ops (op/retain-subtree {})
                            (op/delete-range 1)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/delete-range 1)]))))
      (testing "splits the delete-range if it is wider than 1"
        (is (= [(op/delete-range 1)
                (op/delete-range 1)]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/delete-range 2)])))))
    (testing "replace-value"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val)
               (compose-ops (op/retain-subtree {})
                            (op/replace-value :val)))))
      (testing "on sequences"
        (is (= [(op/replace-value :val)]
               (compose-seqs [(op/retain-subtree {})]
                             [(op/replace-value :val)])))))
    (testing "mark"
      (is (= [(op/mark "id") (op/retain-subtree {})]
             (compose-seqs [(op/retain-subtree {})]
                           [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [(op/retain-subtree {})]
             (compose-seqs [(op/retain-subtree {})]
                           []))))))

(deftest delete-range
  (testing "composing delete-range with"
    (testing "insert-values"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val)
               (compose-ops (op/delete-range 1)
                            (op/insert-values [:val])))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)
                (op/insert-values [:val])]
               (compose-seqs [(op/delete-range 1)]
                             [(op/insert-values [:val])])))))
    (testing "retain-range"
      (testing "on map values or root nodes"
        (is (= (op/delete-range 1)
               (compose-ops (op/delete-range 1)
                            (op/retain-range 1)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)
                (op/retain-range 1)]
               (compose-seqs [(op/delete-range 1)]
                             [(op/retain-range 1)])))))
    (testing "retain-subtree"
      (testing "on map values or root nodes"
        (is (thrown-with-msg?
             Exception
             #":retain-subtree after a :delete-range is invalid"
             (compose-ops (op/delete-range 1)
                          (op/retain-subtree {})))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)
                (op/retain-subtree [])]
               (compose-seqs [(op/delete-range 1)]
                             [(op/retain-subtree [])])))))

    (testing "delete-range"
      (testing "on map values or root nodes"
        (is (= (op/delete-range 1)
               (compose-ops (op/delete-range 1)
                            (op/delete-range 1)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)
                (op/delete-range 1)]
               (compose-seqs [(op/delete-range 1)]
                             [(op/delete-range 1)])))
        (testing "(found through generative testing)"
          (is (= [(op/delete-range 1)]
                 (compose-seqs [(op/delete-range 1)
                                (op/insert-values [:val])]
                               [(op/delete-range 1)]))))))
    (testing "replace-value"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val)
               (compose-ops (op/delete-range 1)
                            (op/replace-value :val)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1) (op/replace-value :val)]
               (compose-seqs [(op/delete-range 1)]
                             [(op/replace-value :val)])))))
    (testing "mark"
      (is (= [(op/delete-range 1) (op/mark "id")]
             (compose-seqs [(op/delete-range 1)]
                           [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [(op/delete-range 1)]
             (compose-seqs [(op/delete-range 1)]
                           []))))))


(deftest replace-value
  (testing "composing replace-value with"
    (testing "insert-values"
      (testing "on map values or root nodes"
        (is (= (op/insert-values [:val2])
               (compose-ops (op/replace-value :val1)
                            (op/insert-values [:val2])))))
      (testing "on sequences"
        (is (= [(op/insert-values [:val2])
                (op/replace-value :val1)]
               (compose-seqs [(op/replace-value :val1)]
                             [(op/insert-values [:val2])])))))
    (testing "retain-range"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val)
               (compose-ops (op/replace-value :val)
                            (op/retain-range 1)))))
      (testing "on sequences"
        (is (= [(op/replace-value :val)]
               (compose-seqs [(op/replace-value :val)]
                             [(op/retain-range 1)]))))
      (testing "splits the retain-range if it is wider than 1"
        (is (= [(op/replace-value :val)
                (op/retain-range 1)]
               (compose-seqs [(op/replace-value :val)]
                             [(op/retain-range 2)])))))
    (testing "retain-subtree"
      (testing "on map values or root nodes"
        (is (= (op/replace-value {:key1 :val1
                                  :key2 :val2})
               (compose-ops (op/replace-value {:key1 :val1})
                            (op/retain-subtree
                             {:key2 (op/insert-values [:val2])})))))
      (testing "on sequences"
        (is (= [(op/replace-value [:val1 :val2])]
               (compose-seqs [(op/replace-value [:val1])]
                             [(op/retain-subtree
                               [(op/retain-range 1)
                                (op/insert-values [:val2])])])))))

    (testing "delete-range"
      (testing "on map values or root nodes"
        (is (= (op/delete-range 1)
               (compose-ops (op/replace-value :val)
                            (op/delete-range 1)))))
      (testing "on sequences"
        (is (= [(op/delete-range 1)]
               (compose-seqs [(op/replace-value :val)]
                             [(op/delete-range 1)]))))
      (testing "splits the retain-range if it is wider than 1"
        (is (= [(op/delete-range 1)
                (op/delete-range 1)]
               (compose-seqs [(op/replace-value :val)]
                             [(op/delete-range 2)])))))
    (testing "replace-value"
      (testing "on map values or root nodes"
        (is (= (op/replace-value :val2)
               (compose-ops (op/replace-value :val1)
                            (op/replace-value :val2)))))
      (testing "on sequences"
        (is (= [(op/replace-value :val2)]
               (compose-seqs [(op/replace-value :val1)]
                             [(op/replace-value :val2)])))))
    (testing "mark"
      (is (= [(op/mark "id") (op/replace-value :val)]
             (compose-seqs [(op/replace-value :val)]
                           [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [(op/replace-value :val)]
             (compose-seqs [(op/replace-value :val)]
                           []))))))

(deftest mark
  (testing "composing mark with"
    (testing "insert-values"
      (is (= [(op/mark "id")
              (op/insert-values [:val1])]
             (compose-seqs [(op/mark "id")]
                           [(op/insert-values [:val1])]))))
    (testing "retain-range"
      (is (= [(op/mark "id")
              (op/retain-range 1)]
             (compose-seqs [(op/mark "id")]
                           [(op/retain-range 1)]))))
    (testing "retain-subtree"
      (is (= [(op/mark "id")
              (op/retain-subtree {})]
             (compose-seqs [(op/mark "id")]
                           [(op/retain-subtree {})]))))
    (testing "delete-range"
      (is (= [(op/mark "id")
              (op/delete-range 1)]
             (compose-seqs [(op/mark "id")]
                           [(op/delete-range 1)]))))
    (testing "replace-value"
      (is (= [(op/mark "id")
              (op/replace-value :val2)]
             (compose-seqs [(op/mark "id")]
                           [(op/replace-value :val2)]))))
    (testing "mark"
      (is (= [(op/mark "id1")
              (op/mark "id2")]
             (compose-seqs [(op/mark "id1")]
                           [(op/mark "id2")]))))
    (testing "end-of-sequence"
      (is (= [(op/mark "id")]
             (compose-seqs [(op/mark "id")]
                           []))))))

(deftest end-of-sequence
  (testing "composing end-of-sequence with"
    (testing "insert-values"
      (is (= [(op/insert-values [:val])]
             (compose-seqs []
                           [(op/insert-values [:val])]))))
    (testing "retain-range"
      (is (= [(op/retain-range 1)]
             (compose-seqs []
                           [(op/retain-range 1)]))))
    (testing "retain-subtree"
      (is (= [(op/retain-subtree {})]
             (compose-seqs []
                           [(op/retain-subtree {})]))))
    (testing "delete-range"
      (is (= [(op/delete-range 1)]
             (compose-seqs []
                           [(op/delete-range 1)]))))
    (testing "replace-value"
      (is (= [(op/replace-value :val2)]
             (compose-seqs []
                           [(op/replace-value :val2)]))))
    (testing "mark"
      (is (= [(op/mark "id")]
             (compose-seqs []
                           [(op/mark "id")]))))))
