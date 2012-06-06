(ns ircumflex.connection
  (:require [ircumflex.message :as msg])
  (:use lamina.core))

(defn filter-by-type
  {:internal true}
  [type ch]
  (filter* #(msg/has-type? % type) ch))

(defn register-connection
  "Send messages to register the IRC client with the IRC server.

   NICK and USER commands are sent to register a nick, login, and real
   name with the connection. The nicks in 'nicks' are tried in turn.
   If one is already in use by another client, the next nick is
   tried."
  [from-server to-server nicks login real-name]
  (let [reg-responses (filter* (comp #{::msg/welcome ::msg/nicknameinuse}
                                     :type)
                               (fork from-server))
        result        (result-channel)]
    (letfn [(step [nicks]
              (if (empty? nicks)
                (enqueue result false)
                (do (enqueue to-server (msg/nick-command (first nicks)))
                    (enqueue to-server (msg/user-command login real-name))
                    (receive reg-responses
                             (fn [{:keys [type]}]
                               (case type
                                 ::msg/welcome       (enqueue result true)
                                 ::msg/nicknameinuse (step (rest nicks)))))))
              result)]
      (step nicks)
      result)))

