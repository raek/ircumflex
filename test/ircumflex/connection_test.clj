(ns ircumflex.connection-test
  (:require [ircumflex.message :as msg])
  (:use ircumflex.connection
        lamina.core
        midje.sweet))

(fact "it responds to pings"
  (let [to-client   (channel)
        from-client (channel)]
    (respond-to-pings to-client from-client)
    (enqueue to-client (msg/ping-command "653913888"))
    (wait-for-message from-client 1000)
    => (msg/pong-command "653913888")))

(fact "registration succeeds when nick and user commands are answered by a welcome"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           ["my_nick"]
                                           "my_login"
                                           "my real name")]
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "my_nick")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/welcome-reply "my_nick" "Welcome!"))
    (wait-for-result registration 1000)
    => truthy))

(fact "registration does not consume messages"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           ["my_nick"]
                                           "my_login"
                                           "my real name")]
    (enqueue to-client (msg/notice-command "AUTH" "*** Looking up your hostname"))
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "my_nick")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/welcome-reply "my_nick" "Welcome!"))
    (wait-for-result registration 1000)
    => truthy
    (close to-client)
    (channel-seq to-client 1000)
    => [(msg/notice-command "AUTH" "*** Looking up your hostname")
        (msg/welcome-reply "my_nick" "Welcome!")]))

(fact "register with another nick if it is in use"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           ["first" "second" "third" "fourth"]
                                           "my_login"
                                           "my real name")]
    (enqueue to-client (msg/notice-command "AUTH" "*** Looking up your hostname"))
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "first")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/nicknameinuse-error "first" "Nickname is already in use."))
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "second")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/nicknameinuse-error "second" "Nickname is already in use."))
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "third")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/welcome-reply "third" "Welcome!"))
    (wait-for-result registration 1000)
    => truthy))

(fact "registration fails when all nicks are in use"
  (let [to-client     (channel)
        from-client   (channel)
        nick-messages (filter-by-type ::msg/nick from-client)
        user-messages (filter-by-type ::msg/user from-client)
        registration  (register-connection to-client from-client
                                           ["my_nick"]
                                           "my_login"
                                           "my real name")]
    (wait-for-message nick-messages 1000)
    => (msg/nick-command "my_nick")
    (wait-for-message user-messages 1000)
    => (msg/user-command "my_login" "my real name")
    (enqueue to-client (msg/nicknameinuse-error "my_nick" "Nickname is already in use."))
    (wait-for-result registration 1000)
    => falsey))

