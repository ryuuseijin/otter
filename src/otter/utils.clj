(ns otter.utils)

(defn panic [message]
  (throw (ex-info message {})))

(defn unreachable []
  (assert false "unreachable code reached"))

(defn unzip [s]
  (cond->> s (seq s) (apply map vector)))

(defn map-kv [key-fn val-fn m]
  (persistent!
   (reduce-kv (fn [r k v]
                (assoc! r (key-fn k) (val-fn v)))
              (transient {})
              m)))

(defn map-vals [val-fn m]
  (map-kv identity val-fn m))

