(ns ircumflex.connection-test
  (:require [ircumflex.message :as msg])
  (:use ircumflex.connection
        lamina.core
        midje.sweet))

(fact "registration succeeds when nick and user commands are answered by a welcome"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           "my_nick"
                                           "my_login"
                                           "my real name")]
    (wait-for-message nick-messages 1000)
    => (contains (msg/nick-command "my_nick"))
    (wait-for-message user-messages 1000)
    => (contains (msg/user-command "my_login" "my real name"))
    (enqueue to-client (msg/welcome-reply "my_nick" "Welcome!"))
    (wait-for-result registration 1000)
    => truthy))

(fact "registration does not consume messages"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           "my_nick"
                                           "my_login"
                                           "my real name")]
    (enqueue to-client (msg/notice-command "AUTH" "*** Looking up your hostname"))
    (wait-for-message nick-messages 1000)
    => (contains (msg/nick-command "my_nick"))
    (wait-for-message user-messages 1000)
    => (contains (msg/user-command "my_login" "my real name"))
    (enqueue to-client (msg/welcome-reply "my_nick" "Welcome!"))
    (wait-for-result registration 1000)
    => truthy
    (close to-client)
    (channel-seq to-client 1000)
    => [(msg/notice-command "AUTH" "*** Looking up your hostname")
        (msg/welcome-reply "my_nick" "Welcome!")]))

