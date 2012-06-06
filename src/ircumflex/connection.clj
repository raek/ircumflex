(ns ircumflex.connection
  (:require [ircumflex.message :as msg])
  (:use lamina.core))

(defn filter-by-type
  {:internal true}
  [type ch]
  (filter* #(msg/has-type? % type) ch))

(defn register-connection
  "Send messages to register the IRC client with the IRC server. "
  [from-server to-server nick login real-name]
  (enqueue to-server (msg/nick-command nick))
  (enqueue to-server (msg/user-command login real-name))
  (let [result (result-channel)]
    (receive (filter-by-type ::msg/welcome (fork from-server))
             (fn [_]
               (enqueue result true)))
    result))

