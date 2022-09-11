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
