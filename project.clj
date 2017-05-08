(defproject clj-playground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]
                 [weasel "0.7.0" :exclusions [org.clojure/clojurescript]]
                 [rum "0.10.8"]]
  :profiles
  {:dev {:repl-options {:init-ns clj-playground.tools
                        :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
         :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                        [com.cemerick/piggieback "0.2.1"]
                        [jonase/eastwood "0.2.3"
                         :exclusions [org.clojure/clojure]]]}})
