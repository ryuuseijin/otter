(ns otter.hub-and-spokes
  (:require [otter.point-to-point :as ptp]))

(defn snapshot [state revision-id]
  {:state state
   :revision-id revision-id})

(defn message [type data]
  {:type type
   :data data})

(defn receive-ptp-message [client-or-server message]
  (let [[new-ptp-state revision] (ptp/receive (:data message))]
    (-> client-or-server
        (update :state materialize (:delta revision))
        (assoc :ptp-state new-ptp-state))))

;; -- client

(defn client [snapshot]
  {:state initial-state
   :process-id (random-id)
   :sequence-number 0
   :ptp-state empty-state})

(defn subscribe [client]
  (message :subscribe (:process-id client)))

(defn generate-revision [client delta]
  (let [revision (revision delta
                           (:process-id client)
                           (:sequence-number client))
        [new-ptp-state ptp-message] (ptp/send (:ptp-state client) revision)])
  [(-> client
       (update :state materialize delta)
       (update :sequence-number inc)
       (assoc :ptp-state new-ptp-state))
   (message :ptp-message ptp-message)])

(defmulti client-receive (fn [client message]
                           (:type message)))

(defmethod client-receive :ptp-message [client message]
  (receive-ptp-message client message))

;; -- server

(defn server [snapshot]
  {:state initial-state
   :ptp-hub {}})

(defmulti server-receive (fn [server message]
                           (:type message)))

(defmethod server-receive :ptp-message [server message]
  (-> server
      (receive-ptp-message message)
      ))


