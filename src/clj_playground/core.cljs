(ns clj-playground.core
  (:require [clj-playground.env :as env]
            clj-playground.repl-conn))

(defn main
  [x]
  (println (str "Hello, " x "!")))

(main "Bob")
