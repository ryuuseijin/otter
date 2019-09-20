(ns otter.delta-test
  (:require [clojure.test :refer :all]
            [otter.operations :as op]
            [otter.delta :refer [optimize optimize-seq optimize-map]]))

(deftest join-ops
  (testing "multiple consecutive ops in a sequence are joined together"
    (testing "(retain-range)"
      (is (= [(op/retain-range 6)
              (op/insert-values [:val])]
             (optimize-seq [(op/retain-range 1)
                            (op/retain-range 2)
                            (op/retain-range 3)
                            (op/insert-values [:val])]))))
    (testing "(delete-range)"
      (is (= [(op/delete-range 6)]
             (optimize-seq [(op/delete-range 1)
                            (op/delete-range 2)
                            (op/delete-range 3)]))))
    (testing "(insert-values)"
      (is (= [(op/insert-values [1 2 3])]
             (optimize-seq [(op/insert-values [1])
                            (op/insert-values [2])
                            (op/insert-values [3])]))))
    (testing "(insert-values with list instead of vector)"
      (is (= [(op/insert-values [1 2 3])]
             (optimize-seq [(op/insert-values (list 1))
                            (op/insert-values (list 2 3))])))))
  (testing "ops of different type are not joined"
    (is (= [(op/retain-range 1)
            (op/delete-range 1)]
           (optimize-seq [(op/retain-range 1)
                          (op/delete-range 1)])))
    (is (= [(op/retain-range 1)
            (op/insert-values [:val])]
           (optimize-seq [(op/retain-range 1)
                          (op/insert-values [:val])])))))

(deftest prune-ops
  (testing "in sequences ops with a width of zero are pruned"
    (testing "(retain-range)"
      (is (= [] (optimize-seq [(op/retain-range 0)]))))
    (testing "(delete-range)"
      (is (= [] (optimize-seq [(op/delete-range 0)]))))
    (testing "(insert-values)"
      (is (= [] (optimize-seq [(op/insert-values [])])))))
  (testing "in maps retain op on values are pruned"
    (is (= {} (optimize-map {:key op/retain}))))
  (testing "retain-range op at the end of a sequence are pruned"
    (is (= [(op/insert-values [:val])]
           (optimize-seq [(op/insert-values [:val])
                          (op/retain-range 1)]))))
  (testing "a subtree of retain-range op is replaced with a single retain-range op"
    (testing "in sequences"
      (is (= op/retain
             (optimize (op/retain-subtree [(op/retain-range 1)])))))
    (testing "in maps"
      (is (= op/retain
             (optimize (op/retain-subtree {:key (op/retain-range 1)})))))))
