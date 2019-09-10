(ns otter.invert-test
  (:require [clojure.test :refer :all]
            [otter.operations :as op]
            [otter.invert :refer [invert-seq invert-map]]))

(deftest insert-values
  (testing "invert insert-values"
    (testing "in sequences"
      (is (= [(op/delete-range 3)]
             (invert-seq [(op/insert-values [1 2 3])]
                         [1 2 3 4]))))
    (testing "in maps"
      (is (= {:key (op/delete-range 1)}
             (invert-map {:key (op/insert-values [:val])}
                         {:key :val}))))))

(deftest retain-range
  (testing "invert retain-range"
    (testing "in sequences"
      (is (= [(op/retain-range 1)]
             (invert-seq [(op/retain-range 1)]
                         [:val]))))
    (testing "in maps"
      (is (= {:key (op/retain-range 1)}
             (invert-map {:key (op/retain-range 1)}
                         {:key :val}))))))

(deftest retain-subtree
  (testing "invert retain-subtree"
    (testing "in sequences"
      (is (= [(op/retain-subtree [(op/insert-values [1 2 3])])]
             (invert-seq [(op/retain-subtree [(op/delete-range 3)])]
                         [[1 2 3]]))))
    (testing "in maps"
      (is (= {:key (op/retain-subtree {:key (op/insert-values [:val])})}
             (invert-map {:key (op/retain-subtree {:key (op/delete-range 1)})}
                         {:key {:key :val}}))))))

(deftest delete-range
  (testing "invert delete-range"
    (testing "in sequences"
      (is (= [(op/insert-values [1 2 3])]
             (invert-seq [(op/delete-range 3)]
                         [1 2 3 4]))))
    (testing "in maps"
      (is (= {:key (op/insert-values [:val])}
             (invert-map {:key (op/delete-range 1)}
                         {:key :val}))))))

(deftest replace-value
  (testing "invert replace-value"
    (testing "in sequences"
      (is (= [(op/replace-value :val1)]
             (invert-seq [(op/replace-value :val2)]
                         [:val1]))))
    (testing "in maps"
      (is (= {:key (op/replace-value :val1)}
             (invert-map {:key (op/replace-value :val2)}
                         {:key :val1}))))))

(deftest mark
  (testing "invert mark"
    (is (= [(op/mark "id")]
           (invert-seq [(op/mark "id")]
                       [])))))
