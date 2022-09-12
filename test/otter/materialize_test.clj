(ns otter.materialize-test
  (:require [otter.materialize :refer [materialize]]
            [clojure.test :refer :all]))

(deftest insert-root-node
  (testing "insert a root node"
    (is (= :root (materialize nil '(insert :root))))))

(deftest retain-sequence
  (testing "retain a sequence"
    (is (= [:value]
           (materialize [:value]
                        '(retain [1]))))))

(deftest insert-into-empty-sequence
  (testing "insert into an empty sequence"
    (is (= [:value]
           (materialize []
                        '(retain [(insert :value)]))))))

(deftest insert-into-empty-map
  (testing "insert into an empty map"
    (is (= {:key :value}
           (materialize {}
                        '(retain {:key (insert :value)}))))))

(deftest delete-in-sequence
  (testing "delete an element from a sequence"
    (is (= []
           (materialize [:value]
                        '(retain [(delete :value)]))))))

(deftest delete-in-map
  (testing "delete an entry from a map"
    (is (= {}
           (materialize {:key :value}
                        '(retain {:key (delete :value)}))))))

(deftest replace-in-sequence
  (testing "replace an element in a sequence"
    (is (= [:replacement]
           (materialize [:value]
                        '(retain [(replace :value :replacement)]))))))

(deftest replace-in-map
  (testing "replace a value in a map"
    (is (= {:key :replacement}
           (materialize {:key :value}
                        '(retain {:key (replace :value :replacement)}))))))

(deftest retain-in-sequence
  (testing "retain range/subtree should skip values in a sequence"
    (is (= [1 2 [] {} :insert 3 4]
           (materialize [1 2 [] {} 3 4]
                        '(retain [2 [] {} (insert :insert)]))))))

(deftest retain-in-map
  (testing "retain in map values should act as a no-ops"
    (is (= {:key1 :val1 :key2 :val2}
           (materialize {:key1 :val1 :key2 :val2}
                        '(retain {:key1 (retain)
                                  :key2 (retain 1)}))))))

(deftest retain-subtree-in-sequences
  (testing "retain subtrees in sequences"
    (is (= [[:val1] {:key2 :val2}]
           (materialize [[] {}]
                        '(retain [[(insert :val1)]
                                  {:key2 (insert :val2)}]))))))

(deftest retain-subtree-in-map-values
  (testing "retain subtrees in map values"
    (is (= {:key3 [:val3] :key4 {:subkey :val4}}
           (materialize {:key3 [] :key4 {}}
                        '(retain {:key3 [(insert :val3)]
                                  :key4 {:subkey (insert :val4)}}))))))
