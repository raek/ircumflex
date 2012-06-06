(ns ircumflex.serialization
  "Translate IRC messages between string and map form.

   The syntax of IRC message strings is defined in RFC 1459. This namespace
   introduces another representation for IRC messages using Clojure maps and
   functions for converting between the two.

   An IRC message has the following anatomy:

   - an optional sender (called \"prefix\" in the RFC),
   - a message type (called \"command\" in the RFC),
   - and zero or more parameters, of which the last one can contain spaces.

   The sender is either a nickname or a server hostname. If it is a nickname,
   it may also have optional login and hostname information attached to it.

   A message line is parsed into a message vector, which has the following
   structure:

   `[[~sender ~login ~hostname] ~type [~@params]]

   where the places at the unquoted symbols have the the meanings

   sender    the sender, without the login and hostname
   login     the login of the sender
   hostname  the hostname of the sender
   type      the message type (the \"command\")
   params    the message parameters

   Examples:

     (line->message \":achilles!ach123@example.com PRIVMSG #ircumflex :Hello there!\")
     => [[\"achilles\" \"ach123\" \"example.com\"]
         \"PRIVMSG\"
         [\"#quiclj\" \"Hello there!\"]]

     (message->line [[\"achilles\" \"ach123\" \"example.com\"]
                     \"PRIVMSG\"
                     [\"#quiclj\" \"Hello there!\"]])
     => \":achilles!ach123@example.com PRIVMSG #ircumflex :Hello there!\"
  ")

(declare parse-sender parse-params)

(def #^{:private true} message-regex
  #"^(?::([^\u0000\u000a\u000d ]+) +)?([^\u0000\u000a\u000d :][^\u0000\u000a\u000d ]+)(?: +([^\u0000\u000a\u000d]+))?$")

(defn line->message
  "Parses an IRC message line into a message vector. Returns nil if the line
  is not a valid IRC message."
  [s]
  (when-let [[_ sender-str type param-str]
             (and s (re-find message-regex s))]
    (let [sender (parse-sender sender-str)
          params (parse-params param-str)]
      (when (and sender params)
        [sender type params]))))

(def #^{:private true} sender-regex
  #"^([^!@]*)(?:!([^@]*))?(?:@(.*))?$")

(defn- parse-sender [sender-str]
  (if sender-str
    (when-let [[_ sender login hostname] (re-find sender-regex sender-str)]
      [sender login hostname])
    [nil nil nil]))

(def #^{:private true} param-regex
  #"(?:(?<!:)[^ :][^ ]*|(?<=:).*)")

(defn- parse-params [param-str]
  (vec (when param-str
         (re-seq param-regex param-str))))

(declare format-sender maybe-prefixed format-params)

(def #^{:private true} illegal-line-char?
  #{\u0000 \newline \return})

(def #^{:private true} illegal-word-char?
  #{\u0000 \newline \return \space})

(def #^{:private true} illegal-prefix-word-char?
  #{\u0000 \newline \return \space \! \@})

(defn message->line
  "Formats an IRC message vector into a string. Returns nil if the message is
  invalid."
  [msg]
  (let [[[sender login hostname] type params] msg
        sender-str (format-sender sender login hostname)
        param-str (format-params params)]
    (when (and (not-any? illegal-word-char? type)
               sender-str type type param-str)
      (str sender-str type param-str))))

(defn- format-sender [sender login hostname]
  (when (and (not-any? illegal-prefix-word-char? sender)
             (not-any? illegal-prefix-word-char? login)
             (not-any? illegal-prefix-word-char? hostname)
             (or sender (not (or login hostname))))
    (if-not sender
      ""
      (str \: sender
           (maybe-prefixed \! login)
           (maybe-prefixed \@ hostname)
           \space))))

(defn- maybe-prefixed [prefix s]
  (if s
    (str prefix s)
    ""))

(defn- format-params [params]
  (if (empty? params)
    ""
    (let [word-params (butlast params)
          rest-param (last params)]
      (when (and (every? #(not-any? illegal-word-char? %) word-params)
                 (every? seq word-params)
                 (not-any? illegal-line-char? rest-param))
        (->> (concat word-params
                     (list (str \: rest-param)))
             (interpose \space)
             (apply str \space))))))
