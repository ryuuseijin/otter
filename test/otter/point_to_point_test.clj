(ns otter.point-to-point-test
  (:refer-clojure :exclude [send])
  (:require [clojure.test :refer :all]
            [otter.core :as ot]
            [otter.operations :as op]
            [otter.point-to-point :as ptp]))

(def insert-a
  (ot/revision nil
               (ot/revision-id "process-a" 0)
               nil
               (ot/delta (op/retain-subtree [(op/insert-values [:A])]))))
(def xformed-insert-a
  (ot/revision nil
               (ot/revision-id "process-a" 0)
               nil
               (ot/delta (op/retain-subtree [(op/retain-range 1)
                                             (op/insert-values [:A])]))))
(def insert-b
  (ot/revision nil
               (ot/revision-id "process-b" 0)
               nil
               (ot/delta (op/retain-subtree [(op/insert-values [:B])]))))
(def xformed-insert-b
  (ot/revision nil
               (ot/revision-id "process-b" 0)
               nil
               (ot/delta (op/retain-subtree [(op/insert-values [:B])
                                             (op/retain-range 1)]))))

(deftest send
  (testing "sending messsages"
    (let [[state-1 message-1] (ptp/send ptp/empty-state insert-a)
          [state-2 message-2] (ptp/send state-1 insert-b)]
      (is (= 1 (count (:unacked state-1))))
      (is (= 1 (:sent state-1)))
      (is (= (ptp/message {:sent 0 :received 0} insert-a)
             message-1))
      (is (= 2 (count (:unacked state-2))))
      (is (= 2 (:sent state-2)))
      (is (= (ptp/message {:sent 1 :received 0} insert-b)
             message-2)))))

(deftest receive
  (testing "receiving messages"
    (let [[state-1 revision-1] (ptp/receive ptp/empty-state
                                            (ptp/message {:sent 0 :received 0}
                                                         insert-a))
          [state-2 revision-2] (ptp/receive state-1
                                            (ptp/message {:sent 1 :received 0}
                                                         insert-b))]
      (is (= 1 (:received state-1)))
      (is (= revision-1 insert-a))
      (is (= 2 (:received state-2)))
      (is (= revision-2 insert-b))
      
      (is (thrown-with-msg?
           Exception
           #"messages must not be out of order or missing"
           (ptp/receive state-1
                        (ptp/message
                         {:sent 0 ;; invalid: state-1 has one received message
                          :received 0}
                         insert-b))))
      (is (thrown-with-msg?
           Exception
           #"sender must not acknowledge more messages than it received"
           (ptp/receive state-1
                        (ptp/message
                         {:sent 1
                          :received 1} ;; invalid: state-1 has never sent a message
                         insert-b)))))))

(deftest send-receive
  (testing "acknowledging sent messages"
    (let [[state-1 _] (ptp/send ptp/empty-state insert-a)
          ;; state-1 now has an unacknowledge operation
          [state-2 revision] (ptp/receive state-1
                                          (ptp/message {:sent 0 :received 1}
                                                       insert-b))]
      (is (= 1 (count (:unacked state-1))))
      (is (= 0 (count (:unacked state-2))))
      (is (= revision insert-b) "expected no transformation to occur")))

  (testing "transforming against unacknowledged revisions"
    (let [[state-1 _] (ptp/send ptp/empty-state insert-a)
          ;; state-1 now has an unacknowledged operation
          [state-2 revision] (ptp/receive state-1
                                          (ptp/message {:sent 0 :received 0}
                                                       insert-b))]
      (is (= 1 (count (:unacked state-1))))
      (is (= [xformed-insert-a] (:unacked state-2)))
      (is (= xformed-insert-b revision)))))
