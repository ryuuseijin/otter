(ns otter.undo
  (:require [otter.core :as ot]
            [otter.invert :refer [invert]]
            [otter.xform :refer [xform-ops xform-context]]
            [otter.compose :refer [compose-ops]]
            [otter.invert :refer [invert] :as invert]
            [otter.delta :refer [optimize]]
            [otter.materialize :refer [materialize]]
            [otter.utils :refer :all]))

(defn empty-state [combine-local?]
  {:backward []
   :forward  []
   :combine-local? combine-local?})

(def left-wins-xform-ctx
  (xform-context :tie-breaker -1
                 :lww-tie-breaker -1))

;; Having operations a and b (b can be applied after a), we need to
;; shift a to the right, across b, so that a can be applied after b.
;;
;; To visualize the process,
;; * we use 0 to denote the empty state,
;; * uppercase letters to denote other states (e.g. A is the resulting
;;   state when operation a is applied to empty state 0, AB is the
;;   result of applying a and b to the empty state, etc.)
;; * and <() is used to show operation application and direction:
;;
;; Starting with the original operations a and b, and the resulting states:
;;
;;   AB <(a) B <(b) 0
;;
;; we inverse b to get bI:
;;
;;   AB <(a) B (bI)> 0
;;                 ^-------- direction changed
;;
;; now a and bI both apply to state B, and therefore we can use xform to
;; get aX and bIX:
;;
;;   A <(bIX) AB <(a) B (bI)> 0 (aX)> A
;;   ^------^                   ^------^
;;           \--------------------------\--- the xformed ops
;;
;; We have the first operation we need, aX which can now be applied to
;; the empty state to get A. To get AB again we need an operation that
;; goes from A to AB. We have bIX wich goes the opposite direction, but
;; we can fix that by inverting it again:
;;
;;  AB <(bIXI) A <(aX) 0
;;
(defn shift-undo-info [undo-info-a undo-info-b]
  (let [a        (:delta undo-info-a)
        b        (:delta undo-info-b)
        bI       (invert b)
        [aX bIX] (xform-ops left-wins-xform-ctx a bI)
        bIXI (invert bIX)]
    [(assoc undo-info-a :delta aX)
     (assoc undo-info-b :delta bIXI)]))

(defn compose-undo-info [undo-info-a undo-info-b]
  (update undo-info-a :delta #(compose-ops (:delta undo-info-b) %)))

(defn bring-local-to-front [history-v]
  (if (or (< (count history-v) 2)
          (:local? (peek history-v)))
    history-v
    (let [[back1 back0] (peek-n 2 history-v)
          history-pop2 (pop-n 2 history-v)
          [new-back1 new-back0] (shift-undo-info back1 back0)]
      (-> history-pop2
          (cond-> (seq history-pop2)
            (cond-> (:local? (peek history-pop2))
                    (conj new-back0)

                    (not (:local? (peek history-pop2)))
                    (peek-replace compose-undo-info new-back0)))
          (conj new-back1)))))

(defn undo-info [delta tree local?]
  {:delta (invert/prepare delta tree)
   :local? local?
   :redoable? false})

(defn record [state undo-info]
  (let [{:keys [combine-local?]} state
        undo-info (update undo-info :delta invert)]
    (-> state
        (cond-> (and (not (:redoable? undo-info))
                     (or (:local? undo-info)
                         (seq (:backward state))))
          (-> (cond-> (:local? undo-info)
                (-> (update :backward bring-local-to-front)
                    (update :forward empty)))
              (update :backward
                      (fn [backward]
                        (if (and (= (:local? undo-info)
                                    (:local? (peek backward)))
                                 (or (not (:local? undo-info))
                                     (combine-local? (peek backward) undo-info)))
                          (peek-replace backward compose-undo-info undo-info)
                          (conj backward undo-info))))))
        (cond-> (or (:redoable? undo-info)
                    (and (seq (:forward state))
                         (not (:local? undo-info))))
          (-> (cond-> (:local? undo-info)
                (update :forward bring-local-to-front))
              (update :forward
                      (fn [forward]
                        (if (or (not (:local? undo-info))
                                (combine-local? undo-info (peek forward)))
                          (peek-replace forward compose-undo-info undo-info)
                          (conj forward undo-info)))))))))

(defn unredo [state direction]
  (let [new-state (update state direction bring-local-to-front)
        undo-info (peek (direction new-state))]
    (if (:local? undo-info)
      [(update new-state direction pop)
       (assoc undo-info :redoable? (= direction :backward))]
      [new-state nil])))

(defn undo [state]
  (unredo state :backward))

(defn redo [state]
  (unredo state :forward))
