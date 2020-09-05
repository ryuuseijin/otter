(ns otter.hub-and-spokes.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::server (s/keys :req [:snapshot :ptp-hub :history]))
(s/def ::client (s/keys :req [:snapshot :ptp-state :pid :sequence-number]))

(s/def ::init-message        (s/keys :req [:type :pid :revision-id :delta]))
(s/def ::error-message       (s/keys :req [:type :pid :errors]))
(s/def ::subscribe-message   (s/keys :req [:type :pid :init-id]))
(s/def ::unsubscribe-message (s/keys :req [:type :pid]))
