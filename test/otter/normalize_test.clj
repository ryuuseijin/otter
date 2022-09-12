(ns otter.normalize-test
  (:require [otter.normalize :refer [normalize]]
            [clojure.test :refer :all]))

;; For root nodes the numerical range is unncessary and removed.
;;   (retain 1) => (retain)
;;
;; In maps, values act like a root node,
;;   {:key (retain 1 }) => {:key (retain)}
;; retains around subtrees are unwrapped,
;;   {:key (retain []}) => {:key []      }
;;   {:key (retain {}}) => {:key {}      }
;; entries that do nothing are removed.
;;   {:key (retain)} => {}
;;
;; In sequences, retains are unwrapped,
;;   [(retain ...)] => [...]
;; empty subtrees become retains,
;;   [[] {} ...] => [(retain 1) (retain 1) ...]
;; sequence ops are joined
;;   [(retain 1) (retain 1)]  => [(retain 2) ...]
;; operations that do nothing are removed.
;;   [0 (insert) (delete) (retain) ...] => [...]

(deftest retain-root
  (testing "keep retain in root position"
    (is (= '(retain)
           (normalize '(retain)))))
  (testing "eliminate redundant numeric range argument in root position"
    (is (= '(retain)
           (normalize '(retain 1))))))

(deftest retain-zero-range-in-seq
  (testing "eliminate zero-width retain in sequence"
    (is (= '[(insert :dummy)] (normalize '[0 (insert :dummy)])))))

(deftest retain-at-end-of-seq
  (testing "eliminate the last numeric range in a sequence"
    (is (= '[(insert :dummy)] (normalize '[(insert :dummy) 4])))))

(deftest delete-in-seq
  (testing "eliminate redundant delete in sequence"
    (is (= '[] (normalize '[(delete)])))))

(deftest insert-in-seq
  (testing "eliminate redundant insert in sequence"
    (is (= '[] (normalize '[(insert)])))))

(deftest unwrap-retain-in-seq
  (testing "unwrap redundant retains in sequence"
    (is (= '[1 (insert :dummy)]
           (normalize '[(retain 1 (insert :dummy))])))))

(deftest retain-subtree-in-seq
  (testing "turn empty subtrees in a sequence to retains"
    (is (= '[1 (insert :dummy)]
           (normalize '[[] (insert :dummy)])))))

(deftest retain-map-value
  (testing "eliminate redundant retain in map value"
    (is (= '{} (normalize '{:key (retain)})))
    (is (= '{} (normalize '{:key (retain 1)})))))

(deftest insert-map-value
  (testing "eliminate redundant insert in map value"
    (is (= '{} (normalize '{:key (insert)})))))

(deftest delete-map-value
  (testing "eliminate redundant delete in map value"
    (is (= '{} (normalize '{:key (delete)})))))

(deftest unwrap-map-value
  (testing "unwrap redundant retain around sequence subtree in map values"
    (is (= '{:key [(insert :value)]}
           (normalize '{:key (retain [(insert :value)])}))))
  (testing "unwrap redundant retain around map subtree in map values"
    (is (= '{:key {:nested-key (insert :value)}}
           (normalize '{:key (retain {:nested-key (insert :value)})})))))
