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
                 :char (first c)
                 :string s})))))

(def ^{:internal true} check-line
  (make-char-checker "\u0000\r\n"))

(def ^{:internal true} line-regex
  #"(?::([^ ]+) +)?([^ :][^ ]*)(?: +(.+))?")

(def ^{:internal true} sender-regex
  #"([^!@]+)?(?:!([^@]*))?(?:@(.*))?")

(def ^{:internal true} param-regex
  #"(?:(?<!:)[^ :][^ ]*|(?<=:).*)")

(defn line->raw
  "Parse an IRC message line into a raw message vector.

   The line should not contain any \\0, \\r or \\n characters.

   Errors (thrown using Slingshot):

   {:type ::illegal-char, :char c, :string s}

   Thrown when the line 's' contains the illegal character 'c'.

   {:type ::syntax-error, :line line}

   Thrown when the line 'line' is not syntactically valid.

   Example:

   (line-raw \":nick!login@example.com PRIVMSG #ircumflex :Message goes here\")
   => [[\"nick\" \"login\" \"example.com\"]
       \"PRIVMSG\"
       [\"#ircumflex\" \"Message goes here\"]]"
  [line]
  (check-line line)
  (if-let [[_ sender-str type param-str]
           (re-matches line-regex line)]
    (let [sender (let [[_ nick login host]
                       (re-matches sender-regex (or sender-str ""))]
                   [nick login host])
          params (vec (re-seq param-regex (or param-str "")))]
      [sender type params])
    (throw+ {:type ::syntax-error
             :line line})))

(defn prohibit-initial-colon
  {:internal true}
  [s]
  (when (.startsWith s ":")
    (throw+ {:type ::illegal-char
             :char \:
             :string s})))

(def ^{:internal true} check-nick
  (make-char-checker "\u0000\r\n!@ "))

(def ^{:internal true} check-login
  (make-char-checker "\u0000\r\n@ "))

(def ^{:internal true} check-host
  (make-char-checker "\u0000\r\n "))

(def ^{:internal true} check-command
  (let [checker (make-char-checker "\u0000\r\n ")]
    #(do (prohibit-initial-colon %)
         (checker %))))

(def ^{:internal true} check-param
  (let [checker (make-char-checker "\u0000\r\n ")]
    #(do (prohibit-initial-colon %)
         (checker %))))

(def ^{:internal true} check-last-param
  (make-char-checker "\u0000\r\n"))

(defmacro str-when
  "Like 'when', but returns \"\" when the condition is false."
  {:internal true}
  [condition & body]
  `(if ~condition
     (do ~@body)
     ""))

(defn raw->line
  "Turn a raw IRC message vector into a message line.

   Example:

   (raw->line [[\"nick\" \"login\" \"example.com\"]
               \"PRIVMSG\"
               [\"#ircumflex\" \"Message goes here\"]])
   => \":nick!login@example.com PRIVMSG #ircumflex :Message goes here\""
  [msg]
  (let [[[nick login host] command params] msg
        sender-str (str-when nick
                     (check-nick nick)
                     (let [login-str (str-when login
                                       (check-login login)
                                       (str "!" login))
                           host-str  (str-when host
                                       (check-host host)
                                       (str "@" host))]
                       (str ":" nick login-str host-str " ")))
        param-strs (if (empty? params)
                     []
                     (let [butlast-param-strs (vec (for [param (pop params)]
                                                     (do (check-param param)
                                                         (str " " param))))
                           last-param         (peek params)]
                       (check-last-param last-param)
                       (conj butlast-param-strs (str " :" last-param))))]
    (check-command command)
    (apply str sender-str command param-strs)))

(defn message->raw
  "Turn an IRC message map into a raw message vector.

   Example:

   (message->raw {:type    ::privmsg
                  :sender  \"nick\"
                  :login   \"login\"
                  :host    \"example.com\"
                  :target  \"#channel\"
                  :message \"Message goes here\"})
   => [[\"nick\" \"login\" \"example.com\"]
       \"PRIVMSG\"
       [\"#channel\" \"Message goes here\"]]"
  [msg]
  (let [{:keys [sender login host type]} msg
        type-str (str/upper-case (name type))
        params [(:target msg) (:message msg)]]
    [[sender login host]
     type-str
     params]))

(defn raw->message
  "Turn a raw IRC message vector into a message map.

   Example:

   (message->raw [[\"nick\" \"login\" \"example.com\"]
                  \"PRIVMSG\"
                  [\"#channel\" \"Message goes here\"]])
   => {:type    ::privmsg
       :sender  \"nick\"
       :login   \"login\"
       :host    \"example.com\"
       :target  \"#channel\"
       :message \"Message goes here\"}"
  [msg]
  (let [[[sender login host] type params] msg
        type-kw (keyword "ircumflex.message"
                         (str/lower-case (name type)))
        [target message] params]
    {:type    type-kw
     :sender  sender
     :login   login
     :host    host
     :target  target
     :message message}))

(defn message->line
  "Turn an IRC message map into a message line.

   Example:

   (message->line {:type    ::msg/privmsg
                   :sender  \"nick\"
                   :login   \"login\"
                   :host    \"example.com\"
                   :target  \"#channel\"
                   :message \"Message goes here\"})
   => \":nick!login@example.com PRIVMSG #channel :Message goes here\""
  [msg]
  (raw->line (message->raw msg)))

(defn line->message
  "Parse an IRC message line into a message map.

   Example:

   (line->message
    \":nick!login@example.com PRIVMSG #channel :Message goes here\")
   => {:type    ::msg/privmsg
       :sender  \"nick\"
       :login   \"login\"
       :host    \"example.com\"
       :target  \"#channel\"
       :message \"Message goes here\"}"
  [line]
  (raw->message (line->raw line)))

