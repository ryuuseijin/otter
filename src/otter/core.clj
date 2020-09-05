(ns otter.core)

(defn delta [root-op]
  {:root-op root-op})

(defn revision-id [process-id sequence-number]
  {:pid process-id
   :seq-n sequence-number})

(defn revision [parent-id id time delta]
  {:parent-id parent-id
   :id id
   :time time
   :opt {}
   :delta delta})
