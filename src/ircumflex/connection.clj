(ns ircumflex.connection
  (:require [ircumflex.message :as msg])
  (:use aleph.tcp
        lamina.core
        gloss.core))

(defn filter-by-type
  {:internal true}
  [type ch]
  (filter* #(msg/has-type? % type) ch))

(defn register-connection
  "Send messages to register the IRC client with the IRC server. "
  [ch nick login real-name]
  (enqueue ch (msg/nick-command nick))
  (enqueue ch (msg/user-command login real-name))
  (let [result (result-channel)]
    (receive ch (fn [msg]
                  (enqueue result (msg/has-type? msg ::msg/welcome))))
    result))

(defn client-channel
  "Get a client channel. "
  [& {:keys [nick host port]
      :or {nick "ircumflex" port 6667}}]
  (let [delimiters ["\r", "\n"]
        frame (string :utf-8 :delimiters delimiters)
        ch (tcp-client {:host host, :port port, :frame frame})]
    ch))
