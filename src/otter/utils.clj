(ns otter.utils)

(defn panic [message]
  (throw (ex-info message {})))

(defn unreachable []
  (assert false "unreachable code reached"))

(defn map-vals
  ([val-fn]
   (map (fn [[k v]]
          [k (val-fn v)])))
  ([val-fn m]
   (into (empty m) (map-vals val-fn) m)))

(defn filter-vals
  ([val-pred?]
   (filter (fn [[k v]]
             (val-pred? v))))
  ([val-pred? m]
   (into (empty m) (filter-vals val-pred?) m)))

(defn stateful-mapcat-xf
  "returns a transducer similar to the 1-arity version of `mapcat` but a state value is
  maintained during the mapcat transduction.
  The given `mc-with-state` function can take one or two arguments:
   1-arity: is passed the current state as argument and returns any remaining values.
   2-arity: is passed the current state and a value and returns the updated state and any additional values."
  ([cat-with-state] (stateful-mapcat-xf cat-with-state nil))
  ([cat-with-state init-state]
   (fn [rf]
     (let [state (volatile! init-state)]
       (fn
         ([result]
          (let [flushed (cat-with-state @state)]
            (vreset! state nil)
            (reduce rf result flushed)))
         ([result val]
          (let [[new-state & flushed] (cat-with-state @state val)]
            (vreset! state new-state)
            (reduce rf result flushed))))))))
