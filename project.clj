(defproject mgrep "0.1.0-SNAPSHOT"
  :description "mgrep implementation in clojure"
  :url "https://github.com/kvafy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]]
  :main mgrep.cmdline
  :aot [mgrep.cmdline])
