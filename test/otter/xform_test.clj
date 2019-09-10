(ns otter.xform-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [otter.operations :as op]
            [otter.xform :refer [xform xform-seqs xform-maps xform-ops]]
            [otter.materialize :refer [materialize]]
            [otter.generators :as otter-gen]))

(def ctx-pos {:tie-breaker 1
              :lww-tie-breaker 1})
(def ctx-neg {:tie-breaker -1
              :lww-tie-breaker -1})

(def convergence-prop
  (prop/for-all [[tree delta-a delta-b]
                 (gen/let [tree (gen/no-shrink
                                 (otter-gen/tree-gen otter-gen/node-gen))
                           num-ops-1 (gen/choose 0 100)
                           num-ops-2 (gen/choose 0 100)
                           delta-a (otter-gen/delta-gen num-ops-1 tree)
                           delta-b (otter-gen/delta-gen num-ops-2 tree)]
                   [tree delta-a delta-b])]
     (let [[delta-a' delta-b']
           (xform ctx-pos delta-a delta-b)
           tree-1 (-> tree
                      (materialize delta-a)
                      (materialize delta-b'))
           tree-2 (-> tree
                      (materialize delta-b)
                      (materialize delta-a'))]
       (is (= tree-1 tree-2)))))

(deftest generative-1
  (testing "retain subtree maps with no common keys (found by generative testing)"
    (is (= [(op/retain-subtree {"" (op/retain-range 1)})
            (op/retain-subtree {"54zZ9L" (op/insert-values [])})]
           (xform-ops ctx-pos
                      (op/retain-subtree {"" (op/retain-range 1)})
                      (op/retain-subtree {"54zZ9L" (op/insert-values [])}))))))

(deftest split-retain-range
  (testing "overlapping retain-range ops are split"
    (testing "(a > b case) (found by generative testing)"
      (is (= [[(op/retain-range 1)
               (op/retain-range 1)]
              [(op/retain-range 1)
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 2)]
                         [(op/retain-range 1)]))))
    (testing "(a < b case)"
      (is (= [[(op/retain-range 1)
               (op/retain-range 1)]
              [(op/retain-range 1)
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 1)]
                         [(op/retain-range 2)]))))))

(deftest split-delete-range
  (testing "overlapping delete-range ops are split"
    (testing "(a > b case)"
      (is (= [[(op/delete-range 1)]
              []]
             (xform-seqs ctx-pos
                         [(op/delete-range 2)]
                         [(op/delete-range 1)]))))
    (testing "(a < b case)"
      (is (= [[]
              [(op/delete-range 1)]]
             (xform-seqs ctx-pos
                         [(op/delete-range 1)]
                         [(op/delete-range 2)]))))))

(deftest split-retain-delete-range
  (testing "overlapping retain-range and delete-range ops are split"
    (testing "(retain-range > delete-range case)"
      (is (= [[(op/retain-range 1)]
              [(op/delete-range 1)
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 2)]
                         [(op/delete-range 1)]))))
    (testing "(retain-range < delete-range case"
      (is (= [[]
              [(op/delete-range 1)
               (op/delete-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 1)]
                         [(op/delete-range 2)]))))
    (testing "(delete-range < retain-range case)"
      (is (= [[(op/delete-range 1)
               (op/retain-range 1)]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/delete-range 1)]
                         [(op/retain-range 2)]))))
    (testing "(delete-range > retain-range case)"
      (is (= [[(op/delete-range 1)
               (op/delete-range 1)]
              []]
             (xform-seqs ctx-pos
                         [(op/delete-range 2)]
                         [(op/retain-range 1)]))))))

(deftest insert-values
  (testing "xform insert-values with"
    (testing "insert-values"
      (testing "on sequences"
        (testing "with positive tie breakers"
          (is (= [[(op/insert-values [:val1]) (op/retain-range 1)]
                  [(op/retain-range 1) (op/insert-values [:val2])]]
                 (xform-seqs ctx-pos
                             [(op/insert-values [:val1])]
                             [(op/insert-values [:val2])]))))
        (testing "with negative tie breakers"
          (is (= [[(op/retain-range 1) (op/insert-values [:val1])]
                  [(op/insert-values [:val2]) (op/retain-range 1)]]
                 (xform-seqs ctx-neg
                             [(op/insert-values [:val1])]
                             [(op/insert-values [:val2])])))))
      (testing "on maps or root nodes"
        (testing "with positive tie breakers"
          (is (= [{:key (op/retain-range 1)}
                  {:key (op/replace-value :val2)}]
                 (xform-maps ctx-pos
                             {:key (op/insert-values [:val1])}
                             {:key (op/insert-values [:val2])}))))
        (testing "with negative tie breakers"
          (is (= [{:key (op/replace-value :val1)}
                  {:key (op/retain-range 1)}]
                 (xform-maps ctx-neg
                             {:key (op/insert-values [:val1])}
                             {:key (op/insert-values [:val2])}))))))
    (testing "retain-range"
      (testing "on sequences"
        (is (= [[(op/insert-values [:val])
                 (op/retain-range 1)]
                [(op/retain-range 1)
                 (op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/insert-values [:val])]
                           [(op/retain-range 1)]))))
      (testing "on maps"
        (is (= [{:key (op/insert-values [:val])}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/insert-values [:val])}
                           {:key (op/retain-range 1)})))))
    (testing "retain-subtree"
      (testing "on sequences"
        (is (= [[(op/insert-values [:val])
                 (op/retain-range 1)]
                [(op/retain-range 1)
                 (op/retain-subtree [])]]
               (xform-seqs ctx-pos
                           [(op/insert-values [:val])]
                           [(op/retain-subtree [])]))))
      (testing "on maps"
        (is (= [{:key (op/insert-values [:val1])}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/insert-values [:val1])}
                           {:key (op/retain-subtree {})})))))
    (testing "delete-range"
      (testing "on sequences"
        (is (= [[(op/insert-values [:val])]
                [(op/retain-range 1)
                 (op/delete-range 1)]]
               (xform-seqs ctx-pos
                           [(op/insert-values [:val])]
                           [(op/delete-range 1)]))))
      (testing "on maps or root nodes"
        (is (= [{:key (op/insert-values [:val])}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/insert-values [:val])}
                           {:key (op/delete-range 1)})))))
    (testing "replace-value"
      (testing "on sequences"
        (is (= [[(op/insert-values [:val1])
                 (op/retain-range 1)]
                [(op/retain-range 1)
                 (op/replace-value :val2)]]
               (xform-seqs ctx-pos
                           [(op/insert-values [:val1])]
                           [(op/replace-value :val2)]))))
      (testing "on maps"
        (testing "with positive tie breakers"
          (is (= [{:key (op/retain-range 1)}
                  {:key (op/replace-value :val2)}]
                 (xform-maps ctx-pos
                             {:key (op/insert-values [:val1])}
                             {:key (op/replace-value :val2)}))))
        (testing "with negative tie breakers"
          (is (= [{:key (op/replace-value :val1)}
                  {:key (op/retain-range 1)}]
                 (xform-maps ctx-neg
                             {:key (op/insert-values [:val1])}
                             {:key (op/replace-value :val2)}))))))
    (testing "mark"
      (is (= [[(op/insert-values [:val])]
              [(op/mark "id")
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/insert-values [:val])]
                         [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [[(op/insert-values [:val])]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/insert-values [:val])]
                         []))))))

(deftest retain-range
  (testing "xform retain-range with"
    (testing "insert-values"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)
                 (op/retain-range 1)]
                [(op/insert-values [:val])
                 (op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-range 1)]
                           [(op/insert-values [:val])]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/insert-values [:val])}]
               (xform-maps ctx-pos
                           {:key (op/retain-range 1)}
                           {:key (op/insert-values [:val])})))))
    (testing "retain-range"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)]
                [(op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-range 1)]
                           [(op/retain-range 1)]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/retain-range 1)}
                           {:key (op/retain-range 1)})))))
    (testing "retain-subtree"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)]
                [(op/retain-subtree [])]]
               (xform-seqs ctx-pos
                           [(op/retain-range 1)]
                           [(op/retain-subtree [])]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/retain-subtree {})}]
               (xform-maps ctx-pos
                           {:key (op/retain-range 1)}
                           {:key (op/retain-subtree {})})))))
    (testing "delete-range"
      (testing "on sequences"
        (is (= [[]
                [(op/delete-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-range 1)]
                           [(op/delete-range 1)]))))
      (testing "on maps or root nodes"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/delete-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/retain-range 1)}
                           {:key (op/delete-range 1)})))))
    (testing "replace-value"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)]
                [(op/replace-value :val)]]
               (xform-seqs ctx-pos
                           [(op/retain-range 1)]
                           [(op/replace-value :val)]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/replace-value :val)}]
               (xform-maps ctx-pos
                           {:key (op/retain-range 1)}
                           {:key (op/replace-value :val)})))))
    (testing "mark"
      (is (= [[(op/retain-range 1)]
              [(op/mark "id")
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 1)]
                         [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [[(op/retain-range 1)]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-range 1)]
                         []))))))

(deftest retain-subtree
  (testing "xform retain-subtree with"
    (testing "insert-values"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)
                 (op/retain-subtree [])]
                [(op/insert-values [:val])
                 (op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-subtree [])]
                           [(op/insert-values [:val])]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/insert-values [:val])}]
               (xform-maps ctx-pos
                           {:key (op/retain-subtree {})}
                           {:key (op/insert-values [:val])})))))
    (testing "retain-range"
      (testing "on sequences"
        (is (= [[(op/retain-subtree [])]
                [(op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-subtree [])]
                           [(op/retain-range 1)]))))
      (testing "on maps"
        (is (= [{:key (op/retain-subtree {})}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/retain-subtree {})}
                           {:key (op/retain-range 1)})))))
    (testing "retain-subtree"
      (testing "on sequences"
        (is (= [[(op/retain-subtree [(op/insert-values [:val1])
                                     (op/retain-range 1)])]
                [(op/retain-subtree [(op/retain-range 1)
                                     (op/insert-values [:val2])])]]
               (xform-seqs ctx-pos
                           [(op/retain-subtree [(op/insert-values [:val1])])]
                           [(op/retain-subtree [(op/insert-values [:val2])])]))))
      (testing "on maps"
        (is (= [{:key (op/retain-subtree {:key (op/retain-range 1)})}
                {:key (op/retain-subtree {:key (op/replace-value :val2)})}]
               (xform-maps ctx-pos
                           {:key (op/retain-subtree {:key (op/insert-values [:val1])})}
                           {:key (op/retain-subtree {:key (op/insert-values [:val2])})})))))
    (testing "delete-range"
      (testing "on sequences"
        (is (= [[]
                [(op/delete-range 1)]]
               (xform-seqs ctx-pos
                           [(op/retain-subtree [])]
                           [(op/delete-range 1)]))))
      (testing "on maps or root nodes"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/delete-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/retain-subtree {})}
                           {:key (op/delete-range 1)})))))
    (testing "replace-value"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)]
                [(op/replace-value :val)]]
               (xform-seqs ctx-pos
                           [(op/retain-subtree [])]
                           [(op/replace-value :val)]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/replace-value :val)}]
               (xform-maps ctx-pos
                           {:key (op/retain-subtree {})}
                           {:key (op/replace-value :val)})))))
    (testing "mark"
      (is (= [[(op/retain-subtree [])]
              [(op/mark "id")
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-subtree [])]
                         [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [[(op/retain-subtree [])]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/retain-subtree [])]
                         []))))))

(deftest delete-range
  (testing "xform delete-range with"
    (testing "insert-values"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)
                 (op/delete-range 1)]
                [(op/insert-values [:val])]]
               (xform-seqs ctx-pos
                           [(op/delete-range 1)]
                           [(op/insert-values [:val])]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/insert-values [:val])}]
               (xform-maps ctx-pos
                           {:key (op/delete-range 1)}
                           {:key (op/insert-values [:val])})))))
    (testing "retain-range"
      (testing "on sequences"
        (is (= [[(op/delete-range 1)]
                []]
               (xform-seqs ctx-pos
                           [(op/delete-range 1)]
                           [(op/retain-range 1)]))))
      (testing "on maps"
        (is (= [{:key (op/delete-range 1)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/delete-range 1)}
                           {:key (op/retain-range 1)})))))
    (testing "retain-subtree"
      (testing "on sequences"
        (is (= [[(op/delete-range 1)]
                []]
               (xform-seqs ctx-pos
                           [(op/delete-range 1)]
                           [(op/retain-subtree [])]))))
      (testing "on maps"
        (is (= [{:key (op/delete-range 1)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/delete-range 1)}
                           {:key (op/retain-subtree {})})))))
    (testing "delete-range"
      (testing "on sequences"
        (is (= [[]
                []]
               (xform-seqs ctx-pos
                           [(op/delete-range 1)]
                           [(op/delete-range 1)]))))
      (testing "on maps or root nodes"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/delete-range 1)}
                           {:key (op/delete-range 1)})))))
    (testing "replace-value"
      (testing "on sequences"
        (is (= [[(op/delete-range 1)]
                []]
               (xform-seqs ctx-pos
                           [(op/delete-range 1)]
                           [(op/replace-value :val)]))))
      (testing "on maps"
        (is (= [{:key (op/retain-range 1)}
                {:key (op/insert-values [:val])}]
               (xform-maps ctx-pos
                           {:key (op/delete-range 1)}
                           {:key (op/replace-value :val)})))))
    (testing "mark"
      (is (= [[(op/delete-range 1)]
              [(op/mark "id")]]
             (xform-seqs ctx-pos
                         [(op/delete-range 1)]
                         [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [[(op/delete-range 1)]
              []]
             (xform-seqs ctx-pos
                         [(op/delete-range 1)]
                         []))))))

(deftest replace-value
  (testing "xform replace-value with"
    (testing "insert-values"
      (testing "on sequences"
        (is (= [[(op/retain-range 1)
                 (op/replace-value :val1)]
                [(op/insert-values [:val2])
                 (op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/replace-value :val1)]
                           [(op/insert-values [:val2])]))))
      (testing "on maps"
        (testing "with positive tie breakers"
          (is (= [{:key (op/retain-range 1)}
                  {:key (op/replace-value :val2)}]
                 (xform-maps ctx-pos
                             {:key (op/replace-value :val1)}
                             {:key (op/insert-values [:val2])}))))
        (testing "with negative tie breakers"
          (is (= [{:key (op/replace-value :val1)}
                  {:key (op/retain-range 1)}]
                 (xform-maps ctx-neg
                             {:key (op/replace-value :val1)}
                             {:key (op/insert-values [:val2])}))))))
    (testing "retain-range"
      (testing "on sequences"
        (is (= [[(op/replace-value :val)]
                [(op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/replace-value :val)]
                           [(op/retain-range 1)]))))
      (testing "on maps"
        (is (= [{:key (op/replace-value :val)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/replace-value :val)}
                           {:key (op/retain-range 1)})))))
    (testing "retain-subtree"
      (testing "on sequences"
        (is (= [[(op/replace-value :val)]
                [(op/retain-range 1)]]
               (xform-seqs ctx-pos
                           [(op/replace-value :val)]
                           [(op/retain-subtree [])]))))
      (testing "on maps"
        (is (= [{:key (op/replace-value :val)}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/replace-value :val)}
                           {:key (op/retain-subtree {})})))))
    (testing "delete-range"
      (testing "on sequences"
        (is (= [[]
                [(op/delete-range 1)]]
               (xform-seqs ctx-pos
                           [(op/replace-value :val)]
                           [(op/delete-range 1)]))))
      (testing "on maps or root nodes"
        (is (= [{:key (op/insert-values [:val])}
                {:key (op/retain-range 1)}]
               (xform-maps ctx-pos
                           {:key (op/replace-value :val)}
                           {:key (op/delete-range 1)})))))
    (testing "replace-value"
      (testing "on sequences"
        (testing "with positive tie breakers"
          (is (= [[(op/retain-range 1)]
                  [(op/replace-value :val2)]]
                 (xform-seqs ctx-pos
                             [(op/replace-value :val1)]
                             [(op/replace-value :val2)]))))
        (testing "with negative tie breakers"
          (is (= [[(op/replace-value :val1)]
                  [(op/retain-range 1)]]
                 (xform-seqs ctx-neg
                             [(op/replace-value :val1)]
                             [(op/replace-value :val2)])))))
      (testing "on maps"
        (testing "with positive tie breakers"
          (is (= [{:key (op/retain-range 1)}
                  {:key (op/replace-value :val2)}]
                 (xform-maps ctx-pos
                             {:key (op/replace-value :val1)}
                             {:key (op/replace-value :val2)}))))
        (testing "with negative tie breakers"
          (is (= [{:key (op/replace-value :val1)}
                  {:key (op/retain-range 1)}]
                 (xform-maps ctx-neg
                             {:key (op/replace-value :val1)}
                             {:key (op/replace-value :val2)}))))))
    (testing "mark"
      (is (= [[(op/replace-value :val)]
              [(op/mark "id")
               (op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/replace-value :val)]
                         [(op/mark "id")]))))
    (testing "end-of-sequence"
      (is (= [[(op/replace-value :val)]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/replace-value :val)]
                         []))))))

(deftest mark
  (testing "xform replace-value with"
    (testing "insert-values"
      (is (= [[(op/mark "id")
               (op/retain-range 1)]
              [(op/insert-values [:val])]]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         [(op/insert-values [:val])]))))
    (testing "retain-range"
      (is (= [[(op/mark "id")
               (op/retain-range 1)]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         [(op/retain-range 1)]))))
    (testing "retain-subtree"
      (is (= [[(op/mark "id")
               (op/retain-range 1)]
              [(op/retain-subtree [])]]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         [(op/retain-subtree [])]))))
    (testing "delete-range"
      (is (= [[(op/mark "id")]
              [(op/delete-range 1)]]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         [(op/delete-range 1)]))))
    (testing "replace-value"
      (is (= [[(op/mark "id")
               (op/retain-range 1)]
              [(op/replace-value :val)]]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         [(op/replace-value :val)]))))
    (testing "mark"
      (is (= [[(op/mark "id1")]
              [(op/mark "id2")]]
             (xform-seqs ctx-pos
                         [(op/mark "id1")]
                         [(op/mark "id2")]))))
    (testing "end-of-sequence"
      (is (= [[(op/mark "id")]
              []]
             (xform-seqs ctx-pos
                         [(op/mark "id")]
                         []))))))

(deftest end-of-sequence
  (testing "xform end-of-sequence with"
    (testing "insert-values"
      (is (= [[(op/retain-range 1)]
              [(op/insert-values [:val])]]
             (xform-seqs ctx-pos
                         []
                         [(op/insert-values [:val])]))))
    (testing "retain-range"
      (is (= [[(op/retain-range 1)]
              [(op/retain-range 1)]]
             (xform-seqs ctx-pos
                         []
                         [(op/retain-range 1)]))))
    (testing "retain-subtree"
      (is (= [[(op/retain-range 1)]
              [(op/retain-subtree [])]]
             (xform-seqs ctx-pos
                         []
                         [(op/retain-subtree [])]))))
    (testing "delete-range"
      (is (= [[]
              [(op/delete-range 1)]]
             (xform-seqs ctx-pos
                         []
                         [(op/delete-range 1)]))))
    (testing "replace-value"
      (is (= [[(op/retain-range 1)]
              [(op/replace-value :val)]]
             (xform-seqs ctx-pos
                         []
                         [(op/replace-value :val)]))))
    (testing "mark"
      (is (= [[]
              [(op/mark "id2")]]
             (xform-seqs ctx-pos
                         []
                         [(op/mark "id2")]))))))
