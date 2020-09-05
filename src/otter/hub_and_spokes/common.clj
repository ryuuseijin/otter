(ns otter.hub-and-spokes.common)

(defn snapshot [state revision-id]
  {:state state
   :revision-id revision-id})

;; server <-> client

(defn ptp-message [pid ptp-message]
  {:type :ptp
   :pid pid
   :ptp-message ptp-message})
