(ns otter.delta-test
  (:require [clojure.test :refer :all]
            [otter.operations :as op]
            [otter.delta :refer [optimize-op]]))

(deftest join-operations
  (testing "multiple consecutive operations in a sequence are joined together"
    (testing "(retain-range)"
      (is (= (op/retain-subtree [(op/retain-range 6)])
             (optimize-op (op/retain-subtree [(op/retain-range 1)
                                              (op/retain-range 2)
                                              (op/retain-range 3)])))))
    (testing "(delete-range)"
      (is (= (op/retain-subtree [(op/delete-range 6)])
             (optimize-op (op/retain-subtree [(op/delete-range 1)
                                              (op/delete-range 2)
                                              (op/delete-range 3)])))))
    (testing "(insert-values)"
      (is (= (op/retain-subtree [(op/insert-values [1 2 3])])
             (optimize-op (op/retain-subtree [(op/insert-values [1])
                                              (op/insert-values [2])
                                              (op/insert-values [3])]))))))
  (testing "operations of different type are not joined"
    (is (= (op/retain-subtree [(op/retain-range 1)
                               (op/delete-range 1)])
           (optimize-op (op/retain-subtree [(op/retain-range 1)
                                            (op/delete-range 1)]))))
    (is (= (op/retain-subtree [(op/retain-range 1)
                               (op/insert-values [:val])])
           (optimize-op (op/retain-subtree [(op/retain-range 1)
                                            (op/insert-values [:val])]))))))

(deftest prune-operations
  (testing "in sequences operations with a width of zero are pruned"
    (testing "(retain-range)"
      (is (= op/retain
             (optimize-op (op/retain-subtree [(op/retain-range 0)])))))
    (testing "(delete-range)"
      (is (= op/retain
             (optimize-op (op/retain-subtree [(op/delete-range 0)])))))
    (testing "(insert-values)"
      (is (= op/retain
             (optimize-op (op/retain-subtree [(op/insert-values [])]))))))
  (testing "in maps retain operations on values are pruned"
    (is (= op/retain
           (optimize-op (op/retain-subtree {:key op/retain})))))
  (testing "retain-range operations at the end of a sequence are pruned"
    (is (= (op/retain-subtree [(op/insert-values [:val])])
           (optimize-op (op/retain-subtree [(op/insert-values [:val])
                                            (op/retain-range 1)]))))))
