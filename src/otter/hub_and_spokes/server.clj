(ns otter.hub-and-spokes.server
  (:require [otter.core :as ot]
            [otter.point-to-point :as ptp]
            [otter.hub-and-spokes.common :as common]
            [otter.utils :refer [unzip]]))

(defn server [snapshot]
  {:snapshot snapshot
   :history []
   :ptp-hub {}})

(defn init-message [pid revision-id init-delta]
  {:type :revision
   :pid pid
   :revision-id revision-id
   :delta init-delta})

(defn error-message [pid errors]
  {:type :error
   :pid pid
   :errors errors})

(defn hub-receive-send [ptp-hub pid ptp-message]
  (let [[received-ptp-state revision] (ptp/receive (get ptp-hub pid)
                                                   ptp-message)
        [pid_ptp-states pid_messages]
        (->> (for [[other-pid ptp-state] ptp-hub]
               (if (= other-pid pid)
                 [[pid received-ptp-state]
                  [pid (ptp/send-ack received-ptp-state)]]
                 (let [[sent-ptp-state ptp-message] (ptp/send ptp-state revision)]
                   [[other-pid sent-ptp-state]
                    [other-pid ptp-message]])))
             unzip)]
    [(into {} pid_ptp-states)
     (into {} pid_messages)
     revision]))

(defmulti receive (fn [server message]
                           (:type message)))

(defmethod receive :subscribe [server message]
  (let [[from-revision & unseen-history]
        (->> (:history server)
             (drop-while #(not= (:id %) (:from message))))]
    (if (and (:from message)
             (not from-revision))
      [server
       [(error-message (:pid message)
                       [{:type :fatal
                         :message "unable to continue from point in history"}])]]

      [(update server :ptp-hub assoc (:pid message) ptp/empty-state)
       (->> unseen-history
            (map #(init-message (:pid message) %)))])))

(defmethod receive :unsubscribe [server message]
  [(update server :ptp-hub dissoc (:pid message))
   []])

(defmethod receive :ptp [server message]
  (if-let [errors (not-empty
                   (ptp/validate-message (get-in server [:ptp-hub (:pid message)])
                                         (:ptp-message message)))]
    [server
     [(error-message (:pid message) errors)]]

    (let [[new-ptp-hub ptp-message-by-pid revision]
          (hub-receive-send (:ptp-hub server)
                            (:pid message)
                            (:ptp-message message))]
      [(assoc server
              :snapshot ()
              :ptp-hub new-ptp-hub)
       (->> ptp-message-by-pid
            (map (fn [[pid ptp-message]]
                   (ptp-message pid ptp-message))))])))

