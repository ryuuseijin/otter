(ns otter.undo
  (:require [otter.invert :refer [invert]]
            [otter.xform :refer [xform-revisions]]
            [otter.compose :refer [compose]]))

(defn empty-undo-state [combine-local?]
  {:backward []
   :forward  []
   :combine-local combine-local?})

(defn compose-undo-info [undo-info-a undo-info-b]
  (-> undo-info-a
      (update :delta compose (:delta undo-info-b))
      (update :inv-delta compose (:inv-delta undo-info-b))))

;;XX avoid doing so many xforms by just filling out all the info
;;   necessary for the inverse inside the delta
(defn xform-undo-info [undo-info-a undo-info-b]
  (let [[inv-delta-a-> delta-b-<]
        (xform (:inv-delta undo-info-a)
               (:delta undo-info-b))

        [delta-a-> delta-b-<>]
        (xform (:delta undo-info-a)
               delta-b-<)

        _ (assert (= delta-b delta-b-<>))

        [inv-delta-a->< inv-delta-b-<]
        (xform inv-delta-a->
               (:inv-delta undo-info-b))

        _ (assert (= inv-delta-a inv-delta-a><))]

    [(assoc undo-info-a
            :delta delta-a->
            :inv-delta inv-delta-a->)
     (assoc undo-info-b
            :delta delta-b-<
            :inv-delta inv-delta-b-<)]))

(defn invert-undo-info [undo-info]
  (assoc undo-info
         :delta (:inv-delta undo-info)
         :inv-delta (:delta undo-info)))

(defn bring-local-forward [state]
  (let [{:keys [backward]} state]
    (if (or (< (count backward) 2)
            (:local? (peek backward)))
      state
      (let [[back1 back0] (peek-n 2 backward)
            backward-minus2 (pop-n 2 backward)
            [new-back1 new-back0] (xform-undo-info back1 back0)]
        (-> backward-minus2
            (cond->
                (seq    backward-minus2) (peek-replace compose-undo-info back0)
                (empty? backward-minus2) (conj back0))
            (conj back1))))))

(defn undo-info [delta tree local?]
  {:delta delta
   :inv-delta (invert delta tree)
   :local local?
   :redoable false})

(defn record [state undo-info]
  (let [new-state (cond-> state (:local? undo-info) (bring-local-forward))
        {:keys [combine-local? backward]} new-state]
    (-> new-state
        (update :backward
                #(if (and (= (:local? undo-info)
                             (:local? (peek backward)))
                          (or (not (:local? undo-info))
                              (combine-local? (peek backward) undo-info)))
                   (peek-replace % compose-undo-info undo-info)
                   (conj % undo-info)))
        (update :forward
                #(if (or (seq %)
                         (:redoable undo-info))
                   (if (and (:local? undo-info)
                            (combine-local? undo-info (peek %)))
                     (peek-replace % compose-undo-info undo-info)
                     (conj % undo-info))
                   %)))))

(defn undo [state]
  (let [{:keys [backward] :as new-state} (bring-local-forward state)
        undo-info (peek backward)]
    (if (:local? undo-info)
      [(update new-state :backward pop)
       (assoc undo-info :redoable true)]
      [new-state nil])))

(defn redo [state]
  )
