(ns otter.point-to-point
  (:refer-clojure :exclude [send])
  (:require [clojure.string :as string]
            [otter.core :as ot]
            [otter.xform :refer [xform-revisions]]
            [otter.utils :refer [panic queue]]))

(defn transform-2d [revision-a revisions-b]
  (reduce (fn [[rev-a revs-b] rev-b]
            (let [[rev-a' rev-b'] (xform-revisions rev-a rev-b)]
              [rev-a' (conj revs-b rev-b')]))
          [revision-a []]
          revisions-b))

(def empty-state {:unacked (queue)
                  :sent 0
                  :received 0})

(defn message [{:keys [sent received] :as state} revision]
  {:revision revision
   :counters [sent received]})

(defn validate [state {[sent-check received-ack] :counters
                       :as message}]
  (cond-> []
    (> received-ack (:sent state))
    (conj "sender must not acknowledge more messages than it received")

    (not= sent-check (:received state))
    (conj "messages must not be out of order or missing")))

(defn send [state revision]
  [(-> state
       (update :unacked conj revision)
       (update :sent inc))
   (message state revision)])

(defn send-ack [state]
  (message state nil))

(defn receive [state {[sent-check received-ack] :counters
                      :keys [revision] ;; may be nil
                      :as message}]
  (when-let [errors (not-empty (validate state message))]
    (panic (string/join "; " errors)))
  (let [keep (- (:sent state) received-ack)
        have (count (:unacked state))
        new-state (-> state
                      (update :unacked #(-> (iterate pop %)
                                            (nth (- have keep))))
                      (update :received inc))]
    (if (nil? revision)
      [new-state nil]
      (let [[new-rev new-unacked-revs] (transform-2d revision (:unacked new-state))]
        [(assoc new-state :unacked new-unacked-revs)
         new-rev]))))
