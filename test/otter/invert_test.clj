(ns otter.invert-test
  (:require [clojure.test :refer :all]
            [otter.operations :as op]
            [otter.invert :refer [invert prepare]]))

(deftest insert-values
  (testing "invert insert-values"
    (testing "in sequences"
      (is (= (op/retain-subtree
              [(op/delete-range 3)])
             (invert (op/retain-subtree
                      [(op/insert-values [1 2 3])])
                     [1 2 3 4]))))
    (testing "in maps"
      (is (= (op/retain-subtree
              {:key (op/delete-range 1)})
             (invert (op/retain-subtree
                      {:key (op/insert-values [:val])})
                     {:key :val}))))))

(deftest retain-range
  (testing "invert retain-range"
    (testing "in sequences"
      (is (= (op/retain-subtree
              [(op/retain-range 1)])
             (invert (op/retain-subtree
                      [(op/retain-range 1)])
                     [:val]))))
    (testing "in maps"
      (is (= (op/retain-subtree
              {:key (op/retain-range 1)})
             (invert (op/retain-subtree
                      {:key (op/retain-range 1)})
                     {:key :val}))))))

(deftest retain-subtree
  (testing "invert retain-subtree"
    (testing "in sequences"
      (is (= (op/retain-subtree
              [(op/retain-subtree [(op/insert-values [1 2 3])])])
             (invert (op/retain-subtree
                      [(op/retain-subtree [(op/delete-range 3)])])
                     [[1 2 3]]))))
    (testing "in maps"
      (is (= (op/retain-subtree
              {:key (op/retain-subtree {:key (op/insert-values [:val])})})
             (invert (op/retain-subtree
                      {:key (op/retain-subtree {:key (op/delete-range 1)})})
                     {:key {:key :val}}))))))

(deftest delete-range
  (testing "invert delete-range"
    (testing "in sequences"
      (is (= (op/retain-subtree
              [(op/insert-values [1 2 3])])
             (invert (op/retain-subtree
                      [(op/delete-range 3)])
                     [1 2 3 4]))))
    (testing "in maps"
      (is (= (op/retain-subtree
              {:key (op/insert-values [:val])})
             (invert (op/retain-subtree
                      {:key (op/delete-range 1)})
                     {:key :val}))))))

(deftest invert-previously-inverted
  (testing "a previously inverted delete-range does not need to be prepared"
    (is (= (op/retain-subtree
            [(op/insert-values [1 2 3])])
           (-> (prepare (op/retain-subtree [(op/delete-range 3)])
                        [1 2 3 4])
               (invert) ;; insert-values
               (invert) ;; delete-range
               (invert) ;; insert-values
               ))))
  (testing "a previously inverted replace-value does not need to be prepared"
    (is (= (op/retain-subtree
            [(op/replace-value :val1)])
           (-> (prepare (op/retain-subtree [(op/replace-value :val2)])
                        [:val1])
               (invert) ;; replace-value :val1
               (invert) ;; replace-value :val2
               (invert) ;; replace-value :val1
               )))))

(deftest replace-value
  (testing "invert replace-value"
    (testing "in sequences"
      (is (= (op/retain-subtree
              [(op/replace-value :val1)])
             (invert (op/retain-subtree
                      [(op/replace-value :val2)])
                     [:val1]))))
    (testing "in maps"
      (is (= (op/retain-subtree
              {:key (op/replace-value :val1)})
             (invert (op/retain-subtree
                      {:key (op/replace-value :val2)})
                     {:key :val1}))))))

(deftest mark
  (testing "invert mark"
    (is (= (op/retain-subtree [(op/mark "id")])
           (invert (op/retain-subtree [(op/mark "id")])
                   [])))))
