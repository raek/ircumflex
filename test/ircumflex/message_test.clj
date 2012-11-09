(ns ircumflex.message-test
  (:use [ircumflex.message :as msg]
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

(defn any-of [& values]
  (partial contains? (set values)))

(fact "can parse command only messages"
  (line->raw "COMMAND")
  => [[nil nil nil] "COMMAND" []])

(fact "can format command only messages"
  (raw->line [[nil nil nil] "COMMAND" []])
  => "COMMAND")

(fact "can parse command with nick prefix"
  (line->raw ":nick COMMAND")
  => [["nick" nil nil] "COMMAND" []])

(fact "can format command with nick prefix"
  (raw->line [["nick" nil nil] "COMMAND" []])
  => ":nick COMMAND")

(fact "can parse command with nick and host prefix"
  (line->raw ":nick@example.com COMMAND")
  => [["nick" nil "example.com"] "COMMAND" []])

(fact "can format command with nick and host prefix"
  (raw->line [["nick" nil "example.com"] "COMMAND" []])
  => ":nick@example.com COMMAND")

(fact "can parse command with nick, login and host prefix"
  (line->raw ":nick!login@example.com COMMAND")
  => [["nick" "login" "example.com"] "COMMAND" []])

(fact "can format command with nick, login and host prefix"
  (raw->line [["nick" "login" "example.com"] "COMMAND" []])
  => ":nick!login@example.com COMMAND")

(fact "can parse command with server prefix"
  (line->raw ":server.example.com COMMAND")
  => [["server.example.com" nil nil] "COMMAND" []])

(fact "can format command with server prefix"
  (raw->line [["server.example.com" nil nil] "COMMAND" []])
  => ":server.example.com COMMAND")

(fact "can parse command with single parameter"
  (line->raw "COMMAND param")
  => [[nil nil nil] "COMMAND" ["param"]])

(fact "can format command with single parameter"
  (raw->line [[nil nil nil] "COMMAND" ["param"]])
  => (any-of "COMMAND param"
             "COMMAND :param"))

(fact "can parse command with some parameters"
  (line->raw "COMMAND one two three")
  => [[nil nil nil] "COMMAND" ["one" "two" "three"]])

(fact "can format command with some parameters"
  (raw->line [[nil nil nil] "COMMAND" ["one" "two" "three"]])
  => (any-of "COMMAND one two three"
             "COMMAND one two :three"))

(fact "can parse command with non-empty rest parameter"
  (line->raw "COMMAND :param")
  => [[nil nil nil] "COMMAND" ["param"]])

(fact "can parse command with empty rest parameter"
  (line->raw "COMMAND :")
  => [[nil nil nil] "COMMAND" [""]])

(fact "can format command with empty rest parameter"
  (raw->line [[nil nil nil] "COMMAND" [""]])
  => "COMMAND :")

(fact "can parse command with rest parameter with spaces"
  (line->raw "COMMAND :one two three")
  => [[nil nil nil] "COMMAND" ["one two three"]])

(fact "can format command with rest parameter with spaces"
  (raw->line [[nil nil nil] "COMMAND" ["one two three"]])
  => "COMMAND :one two three")

(fact "can parse command complete with prefix and parameters"
  (line->raw ":nick!login@example.com COMMAND one two :three three three")
  => [["nick" "login" "example.com"]
      "COMMAND"
      ["one" "two" "three three three"]])

(fact "can format command complete with prefix and parameters"
  (raw->line [["nick" "login" "example.com"]
              "COMMAND"
              ["one" "two" "three three three"]])
  => ":nick!login@example.com COMMAND one two :three three three")

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

(fact "a nick may not contain illegal characters"
  (doseq [char "\0\n\r!@ "]
    (raw->line [[(str char) nil nil] "COMMAND" []])
    => (throws illegal-char-error?)))

(fact "a login may not contain illegal characters"
  (doseq [char "\0\n\r@ "]
    (raw->line [["nick" (str char) "example.com"] "COMMAND" []])
    => (throws illegal-char-error?)))

(fact "a host may not contain illegal characters"
  (doseq [char "\0\n\r "]
    (raw->line [["nick" "login" (str char)] "COMMAND" []])
    => (throws illegal-char-error?)))

(fact "a command may not contain illegal characters"
  (doseq [char "\0\n\r "]
    (raw->line [[nil nil nil] (str char) []])
    => (throws illegal-char-error?)))

(fact "a param may not contain illegal characters"
  (doseq [char "\0\n\r "]
    (raw->line [[nil nil nil] "COMMAND" [(str char) "final"]])
    => (throws illegal-char-error?))
  (doseq [char "\0\n\r"]
    (raw->line [[nil nil nil] "COMMAND" [(str char)]])
    => (throws illegal-char-error?)))

(fact "a command may not begin with a colon"
  (raw->line [[nil nil nil] ":COMMAND" []])
  => (throws illegal-char-error?))

(fact "a non-final param may not begin with a colon"
  (raw->line [[nil nil nil] "COMMAND" [":medial" "final"]])
  => (throws illegal-char-error?))

(fact "can generalize PRIVMSG messages"
  (message->raw {:type    ::msg/privmsg
                 :sender  "nick"
                 :login   "login"
                 :host    "example.com"
                 :target  "#channel"
                 :message "Message goes here"})
  => [["nick" "login" "example.com"]
      "PRIVMSG"
      ["#channel" "Message goes here"]])

(fact "can specialize PRIVMSG messages"
  (raw->message [["nick" "login" "example.com"]
                 "PRIVMSG"
                 ["#channel" "Message goes here"]])
  => {:type    ::msg/privmsg
      :sender  "nick"
      :login   "login"
      :host    "example.com"
      :target  "#channel"
      :message "Message goes here"})

(fact "can format a message map into a line"
  (message->line {:type    ::msg/privmsg
                  :sender  "nick"
                  :login   "login"
                  :host    "example.com"
                  :target  "#channel"
                  :message "Message goes here"})
  => ":nick!login@example.com PRIVMSG #channel :Message goes here")

(fact "can parse a message line into a map"
  (line->message ":nick!login@example.com PRIVMSG #channel :Message goes here")
  => {:type    ::msg/privmsg
      :sender  "nick"
      :login   "login"
      :host    "example.com"
      :target  "#channel"
      :message "Message goes here"})

