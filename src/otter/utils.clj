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

(defn peek-replace [s f & args]
  (-> (pop s)
      (conj (apply f (peek s) args))))

(defn queue []
  (clojure.lang.PersistentQueue/EMPTY))

(defn first-matching [pred s]
  (first (filter pred s)))
