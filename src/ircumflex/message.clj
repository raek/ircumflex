(ns ircumflex.message
  (:require [clojure.string :as str]))

(defn has-type?
  "Test if the message has the given type."
  [msg type]
  (= (:type msg) type))

(defn defmessage*
  [name-sym proto-name doc-name kind params]
  (let [fn-name-sym (symbol (format "%s-%s" (name name-sym) kind))
        full-name   (if (= proto-name doc-name)
                      proto-name
                      (format "%s (%s)" proto-name doc-name))
        docstring   (format "Construct a %s %s message." full-name kind)
        type-kw     (keyword (name (ns-name *ns*))
                             (name name-sym))
        body        (zipmap (cons :type (map keyword params))
                            (cons type-kw params))]
    (list 'defn fn-name-sym docstring params)))

(defmacro defcommand
  [name-sym params]
  (let [proto-name (str/upper-case (name name-sym))]
    (defmessage* name-sym proto-name proto-name "command" params)))

(defmacro defreply
  [proto-name name-sym]
  (let [doc-name (format "RPL_%s" (str/upper-case name-sym))]
    (defmessage* name-sym proto-name doc-name "reply" '[target message])))

(defmacro deferror
  [proto-name name-sym]
  (let [doc-name (format "ERR_%s" (str/upper-case name-sym))]
    (defmessage* name-sym proto-name doc-name "error" '[target message])))

(defcommand nick [nick])
(defcommand user [login real-name])
(defcommand privmsg [target message])
(defcommand notice [target message])
(defcommand ping [token])
(defcommand pong [token])

(defreply "001" welcome)

(deferror "433" nicknameinuse)

