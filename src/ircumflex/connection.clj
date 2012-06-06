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
  (enqueue ch [[nil nil nil] "NICK" [nick]])
  (enqueue ch [[nil nil nil] "USER" 
               [username hostname servername realname]]))

(defn start
  []
  (connect 
    "localhost" 6667 
    (fn [ch] 
      (let [from-server (map* line->message ch)
            to-server (map* message->line (channel))]
        (siphon to-server ch)
        (let [message-ch (splice from-server to-server)]
          (register message-ch)
          (receive-all message-ch #(println (last (get % 2)))))))
    #(println "failed" %)))
