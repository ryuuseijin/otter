(ns otter.point-to-point
  (:refer-clojure :exclude [send])
  (:require [otter.core :as ot]
            [otter.xform :refer [xform-revisions]]
            [otter.utils :refer [panic queue]]))

(defn transform-2d [revision-a revisions-b]
  (reduce xform-revisions revision-a revisions-b))

(def empty-state {:unacked (queue)
                  :sent 0
                  :received 0})

(defn message [revision {:keys [sent received] :as state}]
  {:revision revision
   :counters [sent received]})

(defn get-counters [state {[sent-check received-ack] :counters
                           :as message}]
  (when-not (<= received-ack (:sent state))
    (panic "the number of acknowledged messages must be less or equal than the number of sent messages"))
  (when-not (= sent-check (inc (:received state)))
    (panic "messages must not be out of order and messages must not be missing"))

  {:sent-check sent-check
   :received-ack received-ack})

(defn send [state revision]
  [(-> state
       (update :unacked conj revision)
       (update :sent inc))
   (message revision state)])

(defn receive [state message]
  (let [{:keys [sent-check received-ack]} (get-counters state message)
        keep (- (:sent state) received-ack)
        have (count (:unacked state))
        pop-cnt (- have keep)
        new-state (-> state
                      (update :unacked #(-> (iterate pop %)
                                            (nth pop-cnt)))
                      (assoc :received inc))]
    [new-state
     (transform-2d (:revision message) (:unacked new-state))]))
