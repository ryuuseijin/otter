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
  (xform-context :tie-breaker 1
                 :lww-tie-breaker 1))

;; (1) a<  2  b<  3
;;         4  a<'
;;     b<' 4
(defn shift-undo-info [undo-info-a undo-info-b]
  #_(println "##" (:delta undo-info-a)
           (invert (:delta undo-info-b)))
  (let [[a<' _]
        (xform-ops left-wins-xform-ctx
                   (:delta undo-info-a)
                   (invert (:delta undo-info-b)))

        #_[_ b<']
        #_(xform-ops left-wins-xform-ctx
                   a<'
                   (:delta undo-info-b))]
    #_(println "&&" a<' (:delta undo-info-b) b<')
    [(assoc undo-info-a :delta a<')
     (assoc undo-info-b :delta (invert _) #_b<')]))

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
                        (if (and (:local? undo-info)
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
