(ns otter.hub-and-spokes.example
  (:require [otter.hub-and-spokes.server :as server]
            [otter.hub-and-spokes.client :as client]))

(defn send-server [entities message]
  (send (:server entities)
        (fn [server]
          (let [[new-server messages] (server/receive server message)]
            (doseq [message messages]
              (send (get-in entities [:clients (:pid message)])
                    client/receive
                    message))
            new-server))))

(defn update-client [entities client-id update-fn & args]
  (send (get-in entities [:clients client-id])
        (fn [client]
          (let [[new-client messages] (apply update-fn client args)]
            (doseq [message messages]
              (send-server entities message))
            new-client))))

(defn insert-value [client value]
  (client/generate-revision
   client
   (op/retain-subtree
    [(op/retain-range
      (count (get-in client [:snapshot :state :subtree])))
     (op/insert-values [value])])))

(def server   (agent (server/server)))
(def client-a (agent (client/client)))
(def client-b (agent (client/client)))

(def entities {:server server
               :client {:A client-a
                        :B client-b}})

(send-server entities (client/subscribe client-a))
(send-server entities (client/subscribe client-b))

(update-client entities :A insert-value :Hello)
(update-client entities :B insert-value :World)

;; disconnect/reconnect and catch up with existing history
