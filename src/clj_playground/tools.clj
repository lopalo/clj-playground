(ns clj-playground.tools
  (:require
   cljs.build.api
   [weasel.repl.websocket :as repl-ws]
   [weasel.repl.server :as repl-server]
   [cemerick.piggieback :as piggieback]
   eastwood.lint))

(def compiled-path "resources/public/js/compiled")
(def cljs-repl-port 9600)
(def dev-build-opts {:main 'clj-playground.core
                     :output-dir (str compiled-path "/" "out")
                     :output-to (str compiled-path "/" "main.js")
                     :asset-path "js/compiled/out"
                     :closure-defines
                     {'clj-playground.env/DEVEL true
                      'clj-playground.env/REPL true
                      'clj-playground.env/REPL_PORT cljs-repl-port}
                     :parallel-build true
                     :verbose true})
(def dev-build-args (-> dev-build-opts seq flatten))
(defonce piggieback-repl piggieback/cljs-repl)

(defn dev-build [] (cljs.build.api/build "src" dev-build-opts))

(defn cljs-repl-env []
  (repl-server/stop)
  (repl-ws/repl-env :port cljs-repl-port))

(defn cljs-repl []
  (apply piggieback-repl (cljs-repl-env) dev-build-args))

(defn patched-piggieback
  ([env] (if (= env :patch)
           (cljs-repl)
           (piggieback-repl env)))
  ([env & args] (apply piggieback-repl env args)))

(defn patch-piggieback []
  (alter-var-root
   #'piggieback/cljs-repl
   (constantly patched-piggieback)))

(defn prod-build []
  (cljs.build.api/build
   "src"
   {:main 'clj-playground.core
    :output-dir (str compiled-path "/" "out")
    :output-to (str compiled-path "/" "main.js")
    :parallel-build true
    :optimizations :advanced
    :verbose true}))

(defn lint []
  (eastwood.lint/eastwood {:source-paths ["src"]
                           :test-paths ["test"]
                           :linters [:all]
                           :exclude-linters [:non-clojure-file]}))

(comment
  (patch-piggieback)
  (lint)
  (dev-build)
  (prod-build))
