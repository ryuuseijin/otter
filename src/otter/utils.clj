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

(defn map-vals
  ([val-fn]
   (map (fn [[k v]]
          [k (val-fn v)])))
  ([val-fn m]
   (map-kv identity val-fn m)))

(defn peek-n [n s]
  (loop [n n
         s s
         r '()]
    (if (and (pos? n)
             (seq s))
      (recur (dec n)
             (pop s)
             (cons (peek s) r))
      r)))

(defn pop-n [n s]
  (-> (iterate pop s)
      (nth n)))

