(ns otter.hub-and-spokes.client
  (:require [otter.core :as ot]
            [otter.hub-and-spokes.common :as common]))

(defn client [snapshot pid]
  {:snapshot snapshot
   :pid pid
   :sequence-number 0
   :ptp-state ptp/empty-state})

(defn subscribe-message [pid init-revision-id]
  {:type :subscribe
   :pid pid
   :init-id init-revision-id ;; may be nil
   })

(defn unsubscribe-message [pid]
  {:type :unsubscribe
   :pid pid})

(defn subscribe [client]
  (subscribe-message (:pid client) (-> client :snapshot :revision-id)))

(defn unsubscribe [client]
  (unsubscribe-message (:pid client)))

(defn generate-revision [client delta]
  (let [revision (ot/revision delta (:pid client) (:sequence-number client))
        [new-ptp-state ptp-message] (ptp/send (:ptp-state client) revision)]
    [(-> client
         (update :state materialize (:root-op delta))
         (update :sequence-number inc)
         (assoc :ptp-state new-ptp-state))
     (ptp-message (:pid client) ptp-message)]))

(defmulti receive (fn [client message]
                    (:type message)))

(defmethod receive :ptp [client message]
  (let [[new-ptp-state revision] (ptp/receive (:ptp-state client)
                                              (:data message))]
    (-> client
        (update :state materialize (:root-op (:delta revision)))
        (assoc :ptp-state new-ptp-state))))
