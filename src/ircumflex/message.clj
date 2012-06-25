(ns ircumflex.message
  "This namespace provides functions to work with and convert between
   IRC message representations. There are three different
   representations: message lines (strings), raw messages (vectors),
   and message maps.

   An IRC message has the following anatomy:

   - an optional sender
   - a message type
   - zero or more parameters; the last one may contain spaces

   The sender is either a nickname or a server hostname. If it is a
   nickname, it may also have optional login and hostname information
   attached to it. The meaning of the parameters depend on the message
   type.

   A raw message is a vector with the following structure (all fields
   are strings):

   [[sender login host] type [params...]]

   sender    the sender, without the login and hostname, or nil
   login     the login of the sender or nil
   host      the hostname of the sender or nil
   type      the message type
   params    the message parameters, zero or more"
  (:use [slingshot.slingshot :only [throw+]])
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
   parameters."
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

(defn make-char-checker
  {:internal true}
  [illegal-chars]
  (let [re (re-pattern (str "[\\Q" illegal-chars "\\E]"))]
    (fn [s]
      (when-let [c (re-find re s)]
        (throw+ {:type ::illegal-char
                 :char c
                 :string s})))))

(def ^{:internal true} check-for-illegal-line-char
  (make-char-checker "\u0000\r\n"))

(def ^{:internal true} line-regex
  #"(?::([^ ]+) +)?([^ :][^ ]*)(?: +(.+))?")

(def ^{:internal true} sender-regex
  #"([^!@]+)?(?:!([^@]*))?(?:@(.*))?")

(def ^{:internal true} param-regex
  #"(?:(?<!:)[^ :][^ ]*|(?<=:).*)")

(defn line->raw
  "Parse a IRC message line into a raw message vector.

   The line should not contain any \\0, \\r or \\n characters and should be 

   Errors (thrown using Slingshot):

   {:type ::illegal-char, :char c, :string s}

   Thrown when the line 's' contains the illegal character 'c'.

   {:type ::syntax-error, :line line}

   Thrown when the line 'line' is not syntactically valid.

   Example:

   (line-raw \":nick!user@example.com PRIVMSG #ircumflex :Message goes here\")
   => [[\"nick\" \"user\" \"example.com\"]
       \"PRIVMSG\"
       [\"#ircumflex\" \"Message goes here\"]]"
  [line]
  (check-for-illegal-line-char line)
  (if-let [[_ sender-str type param-str]
           (re-matches line-regex line)]
    (let [sender (let [[_ nick login host]
                       (re-matches sender-regex (or sender-str ""))]
                   [nick login host])
          params (vec (re-seq param-regex (or param-str "")))]
      [sender type params])
    (throw+ {:type ::syntax-error
             :line line})))

