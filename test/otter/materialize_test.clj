(ns otter.materialize-test
  (:require [clojure.test :refer :all]
            [otter.core :as ot]
            [otter.operations :as op]
            [otter.materialize :refer [materialize]]))

(deftest retain
  (testing "retaining"
    (testing "a root value"
      (is (= :val (materialize :val (ot/delta (op/retain-range 1))))))
    (testing "a nil root value"
      (is (nil? (materialize nil (ot/delta (op/retain-range 1))))))
    (testing "a sequence"
      (is (= [:val]
             (materialize [:val]
                          (ot/delta
                           (op/retain-subtree [(op/retain-range 1)]))))))
    (testing "a map"
      (is (= {:key :val}
             (materialize {:key :val}
                          (ot/delta
                           (op/retain-subtree {:key (op/retain-range 1)}))))))
    (testing "a non-existent value in a sequence throws an exception"
      (is (thrown-with-msg?
           Exception
           #"retain-range can only be used to retain existing values"
           (materialize [] (ot/delta
                            (op/retain-subtree [(op/retain-range 1)]))))))
    (testing "a non-existing value in a map"
      (is (= {} (materialize {} (ot/delta
                                 (op/retain-subtree {:key (op/retain-range 1)}))))))))

(deftest insert
  (testing "inserting"
    (testing "a root node"
      (is (= :val
             (materialize nil (ot/delta (op/insert-values [:val])))))
      (testing "throws an exception if the root node is non-nil"
        (is (thrown-with-msg?
             Exception
             #"insert-values can only be used to insert new map entries or nil root nodes"
             (materialize :val (ot/delta (op/insert-values [:val]))))))))
  (testing "a value into an empty"
    (testing "sequence"
      (is (= [:val]
             (materialize [] (ot/delta
                              (op/retain-subtree
                               [(op/insert-values [:val])]))))))
    (testing "map"
      (is (= {:k :val}
             (materialize {} (ot/delta
                              (op/retain-subtree
                               {:k (op/insert-values [:val])})))))))
  (testing "values into a non-empty"
    (testing "sequence"
      (is (= [:one :val1 :two :val2 :three :val3]
             (materialize [:one :two :three]
                          (ot/delta
                           (op/retain-subtree
                            [(op/retain-range 1)
                             (op/insert-values [:val1])
                             (op/retain-range 1)
                             (op/insert-values [:val2])
                             (op/retain-range 1)
                             (op/insert-values [:val3])]))))))
    (testing "map"
      (is (= {:one "one" :two "two" :three "three" :key1 "val1" :key2 "val2"}
             (materialize {:one "one" :two "two" :three "three"}
                          (ot/delta
                           (op/retain-subtree
                            {:key1 (op/insert-values ["val1"])
                             :key2 (op/insert-values ["val2"])})))))))
  (testing "an already existing map values throws an exception"
    (is (thrown-with-msg?
         Exception
         #"insert-values can only be used to insert new map entries or nil root nodes"
         (materialize {:key :val1}
                      (ot/delta
                       (op/retain-subtree
                        {:key (op/insert-values [:val2])})))))))

(deftest delete
  (testing "deleting"
    (testing "a root node"
      (is (nil? (materialize {} (ot/delta (op/delete-range 1))))))
    (testing "a nil root node throws an exception"
      (is (thrown-with-msg?
           Exception
           #"delete-range can only be used with existing map entries or non-nil root nodes"
           (materialize nil (ot/delta (op/delete-range 1))))))
    (testing "from a sequence"
      (is (= []
             (materialize [:val]
                          (ot/delta
                           (op/retain-subtree [(op/delete-range 1)]))))))
    (testing "from a map"
      (is (= {}
             (materialize {:key "val"}
                          (ot/delta
                           (op/retain-subtree {:key (op/delete-range 1)}))))))
    (testing "throws an exception when there is nothing to delete inside a "
      (testing "sequence"
        (is (thrown-with-msg?
             Exception
             #"delete-range can only be used to delete existing values"
             (materialize []
                          (ot/delta
                           (op/retain-subtree
                            [(op/delete-range 1)]))))))
      (testing "map"
        (is (thrown-with-msg?
             Exception
             #"delete-range can only be used with existing map entries"
             (materialize {}
                          (ot/delta
                           (op/retain-subtree
                            {:key (op/delete-range 1)})))))))))

(deftest replace-test
  (testing "replacing"
    (testing "a root node"
      (is (= :val2
             (materialize :val1 (ot/delta (op/replace-value :val2))))))
    (testing "a nil root value throws an exception"
      (is (thrown-with-msg?
           Exception
           #"replace-value can only be used to replace existing map entries or non-nil root nodes"
           (materialize nil (ot/delta (op/replace-value :val))))))
    (testing "a value inside a"
      (testing "sequence"
        (is (= [:val2]
               (materialize [:val1]
                            (ot/delta
                             (op/retain-subtree [(op/replace-value :val2)]))))))
      (testing "map"
        (is (= {:key :val2}
               (materialize {:key :val1}
                            (ot/delta
                             (op/retain-subtree {:key (op/replace-value :val2)})))))))
    (testing "a non-existent value inside a sequence throws an exception"
      (is (thrown-with-msg?
           Exception
           #"replace-value can only be used to replace existing values"
           (materialize []
                        (ot/delta
                         (op/retain-subtree [(op/replace-value :val)]))))))
    (testing "a non-existent value insode a map throws an exception"
      (is (thrown-with-msg?
           Exception
           #"replace-value can only be used to replace existing map entries or non-nil root nodes"
           (materialize {}
                        (ot/delta
                         (op/retain-subtree {:key (op/replace-value :val)}))))))))

(deftest mark
  (testing "marking"
    (testing "a root node throws an exception"
      (is (thrown-with-msg?
           Exception
           #"operations on map values must have a length of exactly 1"
           (materialize :val (ot/delta (op/mark "id"))))))
    (testing "a map value throws an exception"
      (is (thrown-with-msg?
           Exception
           #"operations on map values must have a length of exactly 1"
           (materialize {:key :val}
                        (ot/delta (op/retain-subtree {:key (op/mark "id")}))))))))
