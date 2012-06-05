(ns ircumflex.message)

(defn has-type?
  "Test if the message has the given type."
  [msg type]
  (= (:type msg) type))

(defn nick-command
  "Construct a NICK command message."
  [nick]
  {:type ::nick
   :nick nick})

(defn user-command
  "Construct a USER command message."
  [login real-name]
  {:type ::user
   :login login
   :real-name real-name})

(defn welcome-reply
  "Construct a 001 (RPL_WELCOME) reply message."
  [target message]
  {:type ::welcome
   :target target
   :message message})

