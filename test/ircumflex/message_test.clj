(ns ircumflex.message-test
  (:use ircumflex.message
        midje.sweet))

(fact "has-type? checks the :type entry in the given map"
  (has-type? {:type :abc} :abc)
  => true
  (has-type? {:type :abc} :def)
  => false)

(fact "defcommand functions behave like in the docstring example"
  (defcommand foobar [baz quux])
  (:doc (meta #'foobar-command))
  => "Construct a FOOBAR command message."
  (:arglists (meta #'foobar-command))
  => '([baz quux])
  (foobar-command "x" "y")
  => {:type ::foobar
      :baz "x"
      :quux "y"})

(fact "defreply functions behave like in the docstring example"
  (defreply "200" foobar)
  (:doc (meta #'foobar-reply))
  => "Construct a 200 (RPL_FOOBAR) reply message."
  (:arglists (meta #'foobar-reply))
  => '([target message])
  (foobar-reply "x" "y")
  => {:type ::foobar
      :target "x"
      :message "y"})

(fact "deferror functions behave like in the docstring example"
  (deferror "400" foobar)
  (:doc (meta #'foobar-error))
  => "Construct a 400 (ERR_FOOBAR) error message."
  (:arglists (meta #'foobar-error))
  => '([target message])
  (foobar-error "x" "y")
  => {:type ::foobar
      :target "x"
      :message "y"})

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

