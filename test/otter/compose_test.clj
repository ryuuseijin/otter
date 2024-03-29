(ns otter.compose-test
  (:require [otter.compose :refer [compose]]
            [otter.materialize :refer [materialize]]
            [clojure.test :refer :all]))

(deftest compose-in-seq-insert_insert
  (testing "composing two inserts in a sequence"
    (is (= '(insert :one :two)
           (compose '[(insert :two)]
                    '[(insert :one)])))))

(deftest compose-in-seq_insert_retain
  (testing "composing insert and retain in a sequence"
    (is (= '(retain (insert :one :two))
           (materialize []
                        (compose '(retain (insert :one))
                                 '(retain 1 (insert :two))))))))

(deftest compose-in-seq_insert_delete
  (testing "composing insert and delete in a sequence"
    (is (= '(retain)
           (materialize []
                        (compose '(retain (insert :value))
                                 '(retain (delete :value))))))))
