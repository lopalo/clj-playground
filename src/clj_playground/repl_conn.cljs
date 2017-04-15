
(ns clj-playground.repl-conn
  (:require [clj-playground.env :as env]
            [weasel.repl :as repl]))

(when (identical? env/REPL true)
  (when-not (repl/alive?)
    (repl/connect (str "ws://localhost:" env/REPL_PORT)
                  :print #{:repl :console})))
