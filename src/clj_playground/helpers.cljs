(ns clj-playground.helpers
  (:require [clojure.string :as s]
            [rum.core :as r]))

(defn cursor-collection
  "Can be used only from a render function"
  ([*coll] (cursor-collection *coll (constantly true)))
  ([*coll pred]
   (assert (associative? @*coll))
   (let [coll (r/react *coll)
         indexes (if (map? coll) (keys coll) (-> coll count range))
         reducer
         (fn [coll' idx]
           (if (pred (coll idx))
             (conj coll' [idx (r/cursor-in *coll [idx])])
             coll'))]
     (reduce reducer (empty coll) indexes))))

(defn expand-tag [element]
  (let [wrap (fn [el tag] [tag el])
        split-kw (fn [kw]
                   (as-> kw $ (name $) (s/split $ ">") (map keyword $)))
        [keywords content] (split-with keyword? element)
        [last-kw & rest-kw] (reverse (mapcat split-kw keywords))]
    (reduce wrap (vec (cons last-kw content)) rest-kw)))

(defn nest [& args] (expand-tag (vec args)))

(defn expand-tags [element]
  (letfn [(expand [item]
            (cond
              (seq? item) (map expand-tags item)
              (vector? item) (expand-tags item)
              :default item))]
    (if (vector? element)
      (->> element (mapv expand) expand-tag)
      element)))

(comment
  (nest :div>p>p :p :div.foo>p.bar :a
        {:id 44} [:span "aaa" "bbb"] "ccc"))
