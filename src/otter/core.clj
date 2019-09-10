(ns otter.core)

(defn delta [root-op]
  {:root-op root-op})

(defn revision-id [process-id sequence-number]
  {:process-id process-id
   :sequence-number sequence-number})

(defn revision [delta revision-id time]
  {:delta delta
   :id revision-id
   :time time})
