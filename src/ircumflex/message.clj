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

(defn privmsg-command
  "Construct a PRIVMSG command message."
  [target message]
  {:type ::privmsg
   :target target
   :message message})

(defn notice-command
  "Construct a NOTICE command message."
  [target message]
  {:type ::notice
   :target target
   :message message})

(defn welcome-reply
  "Construct a 001 (RPL_WELCOME) reply message."
  [target message]
  {:type ::welcome
   :target target
   :message message})

(defn nicknameinuse-error
  "Construct a 433 (ERR_NICKNAMEINUSE) error message"
  [target message]
  {:type ::nicknameinuse
   :target target
   :message message})

