(ns clj-playground.core
  (:require [rum.core :as r]
            [clj-playground.app :refer [app]]
            [clj-playground.env :as env]
            clj-playground.repl-conn
            [clj-playground.app-state :refer [*root-state]]))

(defn mount! []
  (r/mount (app *root-state) (. js/document getElementById "app")))

(defn main
  []
  (mount!))

(main)

