(defproject ircumflex "0.1.0-SNAPSHOT"
  :description "An IRC bot/client library for Clojure"
  :url "https://github.com/raek/ircumflex"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[midje "1.4.0"]
                                  [com.stuartsierra/lazytest "1.2.3"]]
                   :repositories {"stuart" "http://stuartsierra.com/maven2"}}}
  :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
  :min-lein-version "2.0.0")
