(ns otter.undo-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [otter.core :as ot]
            [otter.operations :as op]
            [otter.materialize :refer [materialize]]
            [otter.xform :refer [xform-ops xform-context]]
            [otter.invert :refer [invert]]
            [otter.compose :refer [compose-ops]]
            [otter.undo :refer [undo redo record undo-info] :as undo]
            [otter.generators :as otter-gen]
            [otter.utils :refer :all]))

(deftest test-undo-redo
  (testing "undoing and redoing operations"
    (let [undo-state
          (-> (undo/empty-state (constantly false))
              (record (undo-info (op/retain-subtree [(op/insert-values [1 2])])
                                 []
                                 true))
              (record (undo-info (op/retain-subtree [(op/retain-range 2)
                                                     (op/insert-values [3 4])])
                                 [1 2]
                                 true)))
          [state-undo1 undo-info1] (undo undo-state)
          [state-undo2 undo-info2] (undo state-undo1)
          [state-redo1 redo-info1] (-> state-undo2
                                       (record undo-info1)
                                       (record undo-info2)
                                       (redo))
          [state-redo2 redo-info2] (redo state-redo1)]
      (is (= (op/retain-subtree [(op/retain-range 2)
                                 (op/delete-range 2 [3 4])])
             (:delta undo-info1)))
      (is (= (op/retain-subtree [(op/delete-range 2 [1 2])])
             (:delta undo-info2)))
      (is (= (op/retain-subtree [(op/insert-values [1 2])])
             (:delta redo-info1)))
      (is (= (op/retain-subtree [(op/retain-range 2)
                                 (op/insert-values [3 4])])
             (:delta redo-info2))))))

(deftest test-bringing-local-ops-to-the-front
  (testing "local operations are brought to the front and merged"
    (let [undo-state
          (-> (undo/empty-state (constantly true))
              (record (undo-info (op/retain-subtree [(op/insert-values [1 2])])
                                 []
                                 true))
              (record (undo-info (op/retain-subtree [(op/retain-range 2)
                                                     (op/insert-values ["a" "b"])])
                                 [1 2]
                                 false))
              (record (undo-info (op/retain-subtree [(op/retain-range 4)
                                                     (op/insert-values [3 4])])
                                 [1 2 "a" "b"]
                                 true)))
          [state-undo1 undo-info1] (undo undo-state)]
      (is (= (op/retain-subtree [(op/delete-range 2 [1 2])
                                 (op/retain-range 2)
                                 (op/delete-range 2 [3 4])])
             (:delta undo-info1))))))

(deftest test-merging-remote-operations
  (testing "recording remote operations at the front merges them"
    (let [undo-state
          (-> (undo/empty-state (constantly false))
              (record (undo-info (op/retain-subtree []) [] true))
              (record (undo-info (op/retain-subtree [(op/insert-values ["a" "b"])])
                                 []
                                 false))
              (record (undo-info (op/retain-subtree [(op/retain-range 2)
                                                     (op/insert-values ["c" "d"])])
                                 ["a" "b"]
                                 false)))]
      (is (= 2 (count (:backward undo-state))))
      (is (= (op/retain-subtree [(op/delete-range 2 ["a" "b"])
                                 (op/delete-range 2 ["c" "d"])])
             (:delta (peek (:backward undo-state))))))))

(deftest test-moving-back-remote-operations
  (testing "recording a remote operation moves it back across a local operation and merges it with a remote operation behind it"
    (let [undo-state
          (-> (undo/empty-state (fn [undo-info-a undo-info-b]
                                  (:merge? undo-info-a)))
              (record (undo-info op/retain [] true))
              (record (-> (undo-info op/retain [] true)
                          (assoc :merge? true)))
              (record (undo-info (op/retain-subtree [(op/insert-values ["a" "b"])])
                                 []
                                 false))
              ;; this moves forward the previous local undo-record and merges with it
              (record (undo-info (op/retain-subtree [(op/retain-range 2)
                                                     (op/insert-values [1 2])])
                                 ["a" "b"]
                                 true))
              (record (undo-info (op/retain-subtree [(op/retain-range 2)
                                                     (op/insert-values ["c" "d"])])
                                 ["a" "b" 1 2]
                                 false))
              ;; this moves forward the previous local undo-record and merges with it
              ;; and moves back the previous non-local undo-record and causes it to
              ;; be merged into the non-local undo-record before it
              (record (undo-info op/retain [] true)))]
      ;; 6 records - 3 merges = 3
      (is (= 3 (count (:backward undo-state))))
      ;; merged local operation
      (is (= (op/retain-subtree [(op/retain-range 2)
                                 (op/retain-range 2)
                                 (op/delete-range 2 [1 2])])
             (:delta (peek (:backward undo-state)))))
      ;; merged remote operation
      (is (= (op/retain-subtree [(op/delete-range 2 ["a" "b"])
                                 (op/delete-range 2 ["c" "d"])])
             (:delta (peek (pop (:backward undo-state)))))))))

(deftest test-dropping-remote-operations
  (testing "a remote operation that is moved back is dropped if there are no more operations behind it"
    (let [undo-state
          (-> (undo/empty-state (constantly false))
              (record (undo-info op/retain [] true))
              (record (undo-info (op/retain-subtree [(op/insert-values [:val])])
                                 []
                                 false))
              (record (undo-info op/retain [] true)))]
      (is (= 2 (count (:backward undo-state))))
      (is (= [op/retain op/retain]
             (map :delta (:backward undo-state)))))))

(defn undo-actions-gen []
  (gen/let [tree (gen/no-shrink
                  (otter-gen/tree-gen otter-gen/node-gen))
            num-deltas (gen/choose 0 100)
            deltas (otter-gen/deltas-gen tree num-deltas)
            local-remote (gen/vector (gen/elements [:local :remote]) num-deltas)
            unredo (gen/vector (gen/elements [:undo :redo]) 0 100)
            actions (->> (gen/shuffle (concat unredo (repeat (count deltas)
                                                             :record)))
                         (gen/fmap
                          (fn [action-kws]
                            (let [[actions _]
                                  (->> action-kws
                                       (reduce
                                        (fn [[r record-args] action-kw]
                                          (if (= :record action-kw)
                                            [(conj r (cons action-kw (first record-args)))
                                             (next record-args)]
                                            [(conj r [action-kw])
                                             record-args]))
                                        [[] (unzip [deltas local-remote])]))]
                              actions))))]
    [tree actions]))

(defn apply-unredo-actions [tree actions]
  (->> actions
       (reduce (fn [[tree undo-state xform-delta] [action & args]]
                 (case action
                   (:undo :redo)
                   (let [[new-undo-state undo-info]
                         (if (= :undo action)
                           (undo undo-state)
                           (redo undo-state))]
                     (if undo-info
                       [(materialize tree (:delta undo-info))
                        (record new-undo-state undo-info)
                        (compose-ops xform-delta
                                     (:delta undo-info))]
                       [tree new-undo-state xform-delta]))

                   :record
                   (let [[delta local-remote] args
                         [delta' xform-delta']
                         (xform-ops (xform-context :tie-breaker 1
                                                   :lww-tie-breaker 1)
                                    (:root-op delta)
                                    xform-delta)]
                     [(materialize tree delta')
                      (record undo-state
                              (undo-info delta' tree (= local-remote :local)))
                      xform-delta'])))
               [tree
                (undo/empty-state (constantly false))
                op/retain])))

(def undo-prop
  (prop/for-all [[tree actions] (undo-actions-gen)]
     (let [[actioned-tree undo-state] (apply-unredo-actions tree actions)]
       ;; It's already pretty good if no exceptions happen when
       ;; actioning the tree since any bug in the undo implementation
       ;; would snowball defects until materialize/compose/xform throw
       ;; an error, but is there some other property we can check about
       ;; the actioned-tree or the undo-state?
       (is true))))


;;XX allow keeping the redo stack
#_(apply-unredo-actions
 {}
 '[(:record {:root-op {:type :delete-range, :delete-length 1}} :local)
   (:record {:root-op {:type :insert-values, :values [{}]}} :local)
   [:undo]
   (:record {:root-op {:type :replace-value, :value {}}} :local)
   [:redo]
])

;;XX clean up tests
(deftest undo-prop-gen-1
  (testing "whether composition is done in the right order in apply-undo-actions"
    (is (= {}
           (first
            (apply-unredo-actions
             {}
             [[:record (ot/delta (op/replace-value :val2)) :local]
              [:record (ot/delta (op/delete-range 1)) :local]
              [:undo]
              [:undo]]))))))

(deftest undo-prop-gen-2
  (apply-unredo-actions
   :val2
   [[:record (ot/delta (op/delete-range 1)) :local]
    [:record (ot/delta (op/insert-values [:val1])) :local]
    [:record (ot/delta (op/delete-range 1)) :remote]
    [:undo]
    [:undo]]))

(deftest undo-prop-gen-3
  (apply-unredo-actions
   :valz
   [[:record (ot/delta (op/delete-range 1)) :local]
    [:record (ot/delta (op/insert-values [:valq])) :local]
    [:undo]
    [:record (ot/delta (op/replace-value {})) :remote]
    [:record (ot/delta (op/retain-subtree {})) :remote]
    [:record (ot/delta (op/replace-value [])) :remote]
    [:record (ot/delta (op/replace-value :val1)) :remote]
    [:undo]
    ]))

(deftest undo-prop-gen-4
  (apply-unredo-actions
   []
   '[(:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     [:undo]
     [:redo]
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     [:undo]
     [:redo]
     [:undo]
     [:redo]
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :local)
     (:record {:root-op {:type :retain-subtree, :subtree []}}
              :remote)
     [:undo]
     [:undo]
     [:redo]
     [:undo]
     [:undo]
     [:undo]
     [:undo]
     [:undo]
     [:undo]
     [:undo]
     (:record {:root-op {:type :replace-value, :value :val2}}
              :local)
     (:record {:root-op {:type :replace-value, :value {}}}
              :local)
     (:record {:root-op {:type :replace-value, :value {}}}
              :remote)
     [:undo]
     [:undo]
     [:undo]
     ]))

(deftest undo-prop-gen-5
  (testing "local ops win tie breaking"
    (apply-unredo-actions
     []
     '[(:record {:root-op {:type :retain-subtree, :subtree []}}
                :local)
       (:record {:root-op {:type :retain-subtree, :subtree []}}
                :local)
       (:record {:root-op {:type :retain-subtree, :subtree []}}
                :remote)
       (:record {:root-op {:type :replace-value, :value :val2}}
                :local)
       (:record {:root-op {:type :replace-value, :value {}}}
                :remote)
       [:undo]
       [:undo]
       ])))

(deftest undo-prop-gen-6
  (testing "replace-value has its replaced-value property updated"
    (apply-unredo-actions
     {}
     '[
       (:record {:root-op {:type :retain-subtree, :subtree {}}} :local)
       (:record {:root-op {:type :retain-subtree, :subtree {}}} :local)
       (:record {:root-op {:type :retain-subtree,
                           :subtree
                           {"YJDwU" {:type :insert-values, :values [:val1]}}}}
                :remote)
       (:record {:root-op {:type :retain-subtree,
                           :subtree
                           {"YJDwU" {:type :delete-range,
                                     :delete-length 1,
                                     :deleted-values [],
                                     :have-deleted-values? false}}}}
                :local)
       (:record {:root-op {:type :replace-value,
                           :value :val1,
                           :replaced-value nil,
                           :have-replaced-value? false}}
                :remote)
       [:undo]
       [:undo]
       ])))
