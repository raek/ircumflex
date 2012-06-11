(ns ircumflex.message
  (:require [clojure.string :as str]))

(defn has-type?
  "Test if the message has the given type."
  [msg type]
  (= (:type msg) type))

(defn defmessage*
  "Generate the code for a message constructor function definition."
  {:internal true}
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
    (list 'defn fn-name-sym docstring params body)))

(defmacro defcommand
  "Define a command message constructor function.

   A call like (defcommand foobar [baz quux]) expands into the following:

   (defn foobar-command
     \"Construct a FOOBAR command message.\"
     [baz quux]
     {:type ::foobar
      :baz baz
      :quux quux})

   If 'name-sym' is foobar and 'params' [baz quux], then it is assumed
   that the corresponding function name is foobar-command, that the
   type keyword is ::foobar, and that the message type is FOOBAR in
   the IRC specification. The constructor function will have [baz
   quux] as its formal parameters and will, when called, return a map
   where :baz and :quux are associated with the corresponding actual
   paramters."
  {:internal true}
  [name-sym params]
  (let [proto-name (str/upper-case (name name-sym))]
    (defmessage* name-sym proto-name proto-name "command" params)))

(defmacro defreply
  "Define a reply message constructor function.

   A call like (defreply \"200\" foobar) expands into the following:

   (defn foobar-reply
     \"Construct a 200 (RPL_FOOBAR) reply message.\"
     [target message]
     {:type ::foobar
      :target target
      :message message})

   If 'numeric' is \"200\" and 'name-sym' is foobar, then it is
   assumed that the corresponding function name is foobar-reply, that
   the type keyword is ::foobar, and that the message type is 200 in
   the IRC specification (where it is documented with the symbolic
   name RPL_FOOBAR). The constructor function always take two
   arguments which are put under the :target and :message key."
  {:internal true}
  [numeric name-sym]
  (let [doc-name (format "RPL_%s" (str/upper-case name-sym))]
    (defmessage* name-sym numeric doc-name "reply" '[target message])))

(defmacro deferror
  "Define an error message constructor function.

   A call like (deferror \"400\" foobar) expands into the following:

   (defn foobar-error
     \"Construct a 400 (ERR_FOOBAR) error message.\"
     [target message]
     {:type ::foobar
      :target target
      :message message})

   If 'numeric' is \"400\" and 'name-sym' is foobar, then it is
   assumed that the corresponding function name is foobar-error, that
   the type keyword is ::foobar, and that the message type is 400 in
   the IRC specification (where it is documented with the symbolic
   name ERR_FOOBAR). The constructor function always take two
   arguments which are put under the :target and :message key."
  {:internal true}
  [numeric name-sym]
  (let [doc-name (format "ERR_%s" (str/upper-case name-sym))]
    (defmessage* name-sym numeric doc-name "error" '[target message])))

(defcommand nick [nick])
(defcommand user [login real-name])
(defcommand privmsg [target message])
(defcommand notice [target message])
(defcommand ping [token])
(defcommand pong [token])

(defreply "001" welcome)

(deferror "433" nicknameinuse)

