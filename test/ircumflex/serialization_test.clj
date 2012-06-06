(ns ircumflex.serialization-test
  {:author "Rasmus Svensson (raek)"}
  (:use clojure.test)
  (:use [ircumflex.serialization :only (line->message message->line)]))

(defn in? [x coll]
  (contains? coll x))

(deftest test-line->message

  (testing "nil in, nil out"
    (is (nil? (line->message nil))))

  (testing "command only"
    (is (= (line->message "COMMAND")
           [[nil nil nil] "COMMAND" []])))

  (testing "nick prefix"
    (is (= (line->message ":nick COMMAND")
           [["nick" nil nil] "COMMAND" []])))

  (testing "nick and login prefix"
    (is (= (line->message ":nick!login COMMAND")
           [["nick" "login" nil] "COMMAND" []])))

  (testing "nick and hostname prefix"
    (is (= (line->message ":nick@users.example.com COMMAND")
           [["nick" nil "users.example.com"] "COMMAND" []])))

  (testing "nick, login and hostname prefix"
    (is (= (line->message ":nick!login@users.example.com COMMAND")
           [["nick" "login" "users.example.com"] "COMMAND" []])))

  (testing "server prefix"
    (is (= (line->message ":server.example.com COMMAND")
           [["server.example.com" nil nil] "COMMAND" []])))

  (testing "single parameter"
    (is (= (line->message "COMMAND param")
           [[nil nil nil] "COMMAND" ["param"]])))

  (testing "some parameters"
    (is (= (line->message "COMMAND one two three")
           [[nil nil nil] "COMMAND" ["one" "two" "three"]])))

  (testing "some parameters and rest parameter"
    (is (= (line->message "COMMAND one two :three")
           [[nil nil nil] "COMMAND" ["one" "two" "three"]])))

  (testing "non-empty rest parameter"
    (is (= (line->message "COMMAND :param")
           [[nil nil nil] "COMMAND" ["param"]])))

  (testing "empty rest parameter"
    (is (= (line->message "COMMAND :")
           [[nil nil nil] "COMMAND" [""]])))

  (testing "rest parameter with spaces"
    (is (= (line->message "COMMAND :one two three")
           [[nil nil nil] "COMMAND" ["one two three"]])))

  (testing "complete prefix and parameters"
    (is (= (line->message ":nick!login@users.example.com COMMAND one two :three three three")
           [["nick" "login" "users.example.com"] "COMMAND" ["one" "two" "three three three"]])))

  (testing "null in string"
    (is (nil? (line->message "FOO\u0000BAR"))))

  (testing "line feed in string"
    (is (nil? (line->message "FOO\nBAR"))))

  (testing "carriage return in string"
    (is (nil? (line->message "FOO\rBAR"))))

  (testing "no command"
    (is (nil? (line->message ":nick")))))

(deftest test-message->line

  (testing "nil in, nil out"
    (is (nil? (message->line nil))))

  (testing "command only"
    (is (= (message->line [[nil nil nil] "COMMAND" []])
           "COMMAND")))

  (testing "nick prefix"
    (is (= (message->line [["nick" nil nil] "COMMAND" []])
           ":nick COMMAND")))

  (testing "nick and login prefix"
    (is (= (message->line [["nick" "login" nil] "COMMAND" []])
           ":nick!login COMMAND")))

  (testing "nick and hostname prefix"
    (is (= (message->line [["nick" nil "users.example.com"] "COMMAND" []])
           ":nick@users.example.com COMMAND")))

  (testing "nick, login and hostname prefix"
    (is (= (message->line [["nick" "login" "users.example.com"] "COMMAND" []])
           ":nick!login@users.example.com COMMAND")))

  (testing "server prefix"
    (is (= (message->line [["server.example.com" nil nil] "COMMAND" []])
           ":server.example.com COMMAND")))

  (testing "single parameter"
    (is (in? (message->line [[nil nil nil] "COMMAND" ["param"]])
             #{"COMMAND param"
               "COMMAND :param"})))

  (testing "some parameters"
    (is (in? (message->line [[nil nil nil] "COMMAND" ["one" "two" "three"]])
             #{"COMMAND one two three"
               "COMMAND one two :three"})))

  (testing "empty parameter"
    (is (= (message->line [[nil nil nil] "COMMAND" [""]])
           "COMMAND :")))

  (testing "complete prefix and parameters"
    (is (= (message->line [["nick" "login" "users.example.com"] "COMMAND" ["one" "two" "three three three"]])
           ":nick!login@users.example.com COMMAND one two :three three three")))

  (testing "login prefix"
    (is (nil? (message->line [[nil "login" nil] "COMMAND" []]))))

  (testing "hostname prefix"
    (is (nil? (message->line [[nil "login" nil] "COMMAND" []]))))

  (testing "login and hostname prefix"
    (is (nil? (message->line [[nil "login" "users.example.com"] "COMMAND" []]))))

  (testing "empty non-last parameter"
    (is (nil? (message->line [[nil nil nil] "COMMAND" ["" "last"]]))))

  (testing "non-last parameter with space"
    (is (nil? (message->line [[nil nil nil] "COMMAND" ["foo bar" "last"]]))))

  (testing "illegal char in nick"
    (is (nil? (message->line [["foo bar" nil nil] "COMMAND" []]))))

  (testing "illegal char in login"
    (is (nil? (message->line [[nil "foo bar" nil] "COMMAND" []]))))

  (testing "illegal char in hostname"
    (is (nil? (message->line [[nil nil "foo bar"] "COMMAND" []]))))

  (testing "illegal char in command"
    (is (nil? (message->line [[nil nil nil] "FOO\nBAR" []]))))

  (testing "illegal char in non-last parameter"
    (is (nil? (message->line [[nil nil nil] "COMMAND" ["foo\nbar" "last"]]))))

  (testing "illegal char in last parameter"
    (is (nil? (message->line [[nil nil nil] "COMMAND" ["first" "foo\nbar"]])))))
