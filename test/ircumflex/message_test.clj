(ns ircumflex.message-test
  (:use ircumflex.message
        midje.sweet))

(defn error-checker [type]
  (fn [x]
    (= (-> x ex-data :object :type)
       type)))

(def illegal-char-error?
  (error-checker :ircumflex.message/illegal-char))

(def syntax-error?
  (error-checker :ircumflex.message/syntax-error))

(fact "can parse command only messages"
  (line->raw "COMMAND")
  => [[nil nil nil] "COMMAND" []])

(fact "can parse command with nick prefix"
  (line->raw ":nick COMMAND")
  => [["nick" nil nil] "COMMAND" []])

(fact "can parse command with nick and host prefix"
  (line->raw ":nick@example.com COMMAND")
  => [["nick" nil "example.com"] "COMMAND" []])

(fact "can parse command with nick, login and host prefix"
  (line->raw ":nick!login@example.com COMMAND")
  => [["nick" "login" "example.com"] "COMMAND" []])

(fact "can parse command with server prefix"
  (line->raw ":server.example.com COMMAND")
  => [["server.example.com" nil nil] "COMMAND" []])

(fact "can parse command with single parameter"
  (line->raw "COMMAND param")
  => [[nil nil nil] "COMMAND" ["param"]])

(fact "can parse command with some parameters"
  (line->raw "COMMAND one two three")
  => [[nil nil nil] "COMMAND" ["one" "two" "three"]])

(fact "can parse command with non-empty rest parameter"
  (line->raw "COMMAND :param")
  => [[nil nil nil] "COMMAND" ["param"]])

(fact "can parse command with empty rest parameter"
  (line->raw "COMMAND :")
  => [[nil nil nil] "COMMAND" [""]])

(fact "can parse command with rest parameter with spaces"
  (line->raw "COMMAND :one two three")
  => [[nil nil nil] "COMMAND" ["one two three"]])

(fact "can parse command complete with prefix and parameters"
  (line->raw ":nick!login@example.com COMMAND one two :three three three")
  => [["nick" "login" "example.com"]
      "COMMAND"
      ["one" "two" "three three three"]])

(fact "null characters are not allowed in message lines"
  (line->raw "COMMAND\0")
  => (throws illegal-char-error?))

(fact "newline characters are not allowed in message lines"
  (line->raw "COMMAND\n")
  => (throws illegal-char-error?))

(fact "return characters are not allowed in message lines"
  (line->raw "COMMAND\r")
  => (throws illegal-char-error?))

(fact "throws an syntax-error on syntactically invalid lines"
  (line->raw ":nick")
  => (throws syntax-error?))

