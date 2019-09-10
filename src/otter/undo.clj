(ns otter.undo
  (:require [otter.invert :refer [invert]]
            [otter.xform :refer [xform-revisions]]
            [otter.compose :refer [compose]]))

(def empty-undo-state
  {:backward []
   :forward  []})

(defn record [state delta local?]
  (if (seq (:forward state))
    (update state :forward #(-> %
                                (pop)
                                (conj (update (peek %) :delta (compose delta)))))
    (update state :forward conj {:local? local? :delta delta})
    (if (and (seq (:backward state))
             (let [{prev-local? :local?} (peek (:backward state))]
               (= prev-local? local?)))
      (update state :backward #(-> %
                                   (pop)
                                   (conj (update (peek %) :delta (compose delta)))))
      (update state :backward conj {:local? local? :delta delta}))))

(defn undo [state]
  )

(defn redo [state]
  )
