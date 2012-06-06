(ns ircumflex.connection
  (:require [ircumflex.message :as msg])
  (:use ircumflex.serialization
        aleph.tcp
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

(defn irc-client
  [host port]
  (let [delimiters ["\r", "\n"]
        frame (string :utf-8 :delimiters delimiters)
        ch (tcp-client {:host host, :port port, :frame frame})]
    ch))

(defn connect
  [host port success failure]
  (on-realized (irc-client host port) success failure))

(defn register
  [ch & {:keys [nick username hostname servername realname]
         :or {nick "ircumflex"
              username "ircumflex" 
              hostname "localhost" 
              servername "localhost" 
              realname "ircumflex"}}]
  (message->server ch [[nil nil nil] "NICK" [nick]])
  (message->server ch [[nil nil nil] "USER" 
                       [username hostname servername realname]]))

(defn start
  []
  (connect 
    "localhost" 6667 
    (fn [ch] 
      (register ch)
      (receive-all ch #(println %)))
    #(println "failed" %)))

(defn message->server
  "Send message to server."
  [ch message]
  (let [line (message->line message)]
    (enqueue ch line)))

(defn server->message
  "Get message from server. "
  [ch callback]
  (receive ch (fn [line] (callback (line->message line)))))
