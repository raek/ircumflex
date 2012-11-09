(ns ircumflex.parser-test
  (:use 
    clojure.test
    ircumflex.parser))

(deftest parser-test
  (is (= (parse-message ":amalloy!~amalloy@li231-96.members.linode.com PRIVMSG #4clojure")
         {:command "PRIVMSG",
          :params ["#4clojure"],
          :raw ":amalloy!~amalloy@li231-96.members.linode.com PRIVMSG #4clojure",
          :host "li231-96.members.linode.com",
          :user "~amalloy",
          :nick "amalloy"}))
  (is (= (parse-message ":amalloy!~amalloy@li231-96.members.linode.com PRIVMSG #4clojure :ckirkendall: would you mind using amalloy on irc, rather than alan? it's nice to get notifications from my irc client")
         {:command "PRIVMSG",
          :params ["#4clojure" "ckirkendall: would you mind using amalloy on irc, rather than alan? it's nice to get notifications from my irc client"],
          :raw ":amalloy!~amalloy@li231-96.members.linode.com PRIVMSG #4clojure :ckirkendall: would you mind using amalloy on irc, rather than alan? it's nice to get notifications from my irc client",
          :host "li231-96.members.linode.com",
          :user "~amalloy",
          :nick "amalloy"})))
