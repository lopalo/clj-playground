(ns clj-playground.monad
  (:require [clojure.test :as t]))

(set! *warn-on-reflection* true)

(defprotocol Monad
  (bind [this reaction])
  (tail-call [this]))

(def >>= bind)

(defmacro >> [monad monad']
  `(bind ~monad (fn [~'_] ~monad')))

(defn >>' [monad monad']
  (>> monad monad'))

(defn =<< [reaction monad]
  (bind monad reaction))

(def tc tail-call)

(defn |> [monad sym]
  (assert nil "|> used not in (do> ...) block"))

(defn <| [sym monad]
  (assert nil "<| used not in (do> ...) block"))

(defn let> [bindings]
  (assert nil "let> used not in (do> ...) block"))

(declare do>*)

(defmulti expand-do-notation
  (fn [form forms]
    (cond
      (empty? forms) ::do-end
      (and (seq? form) (symbol? (first form))) (-> form first resolve)
      :else :default)))

(defmethod expand-do-notation #'|> [[_ monad sym] forms]
  `(>>= ~monad (fn [~sym] ~(do>* forms))))

(defmethod expand-do-notation #'<| [[_ sym monad] forms]
  (expand-do-notation `(|> ~monad ~sym) forms))

(defmethod expand-do-notation #'let> [[_ bindings] forms]
  `(let [~@bindings] ~(do>* forms)))

(defmethod expand-do-notation ::do-end [form _] form)

(defmethod expand-do-notation :default [form forms]
  `(>> ~form ~(do>* forms)))

(defn do>* [forms]
  (expand-do-notation (first forms) (rest forms)))

(defmacro do> [& body]
  (do>* body))

(defrecord TailCall [function])

(defn tail-call-loop [value]
  (if (instance? TailCall value)
    (recur ((.function ^TailCall value)))
    value))

(defrecord State [computation]
  Monad
  (bind [this reaction]
    (State. (fn [state]
              (let [[value state'] (this state)
                    computation' (.computation ^State (reaction value))]
                (computation' state')))))
  (tail-call [this]
    (State. (fn [state]
              (TailCall. #(computation state)))))
  clojure.lang.IFn
  (invoke [this state]
    (-> state computation tail-call-loop)))

(defn ->state [value]
  (State. (fn [state] [value state])))

(defrecord Cont [computation]
  Monad
  (bind [this reaction]
    (Cont. (fn [cont]
             (this (fn [value]
                     ((reaction value) cont))))))
  clojure.lang.IFn
  (invoke [this cont]
    (computation cont)))

(defn ->cont [value]
  (Cont. (fn [cont] (cont value))))

(defn call-cc [f]
  (Cont. (fn [cont] ((f (fn [value]
                          (Cont. (fn [_] (cont value)))))
                     cont))))

(t/deftest base-test
  (t/testing "State"
    (let [st-peek
          (State. (fn [s] [(peek s) s]))
          st-pop
          (State. (fn [s] [(peek s) (pop s)]))
          st-push
          (fn [value]
            (State. (fn [s] [nil (conj s value)])))]
      (t/testing "Base"
        (let [st-m
              (>>=
               st-pop
               (fn [a]
                 (>>'
                  (st-push :a)
                  (>>
                   (st-push :b)
                   (=<<
                    (fn [b]
                      (->state [(+ a 100) b]))
                    st-peek)))))
              [res stack] (st-m [1 2 3])]
          (t/is (= [103 :b] res))
          (t/is (= [1 2 :a :b] stack))))
      (t/testing "Do block"
        (let [f (fn [t]
                  (do>
                   st-pop
                   (st-push :a)
                   (let> [u 33
                          i 5])
                   (st-push 100)
                   (>>= st-peek #(st-push (+ % u i)))
                   (>> (st-push t) (st-push :b))
                   (st-push :c)
                   (<| a st-pop)
                   (<| b st-peek)
                   (->state [a b])))
              [res stack] ((f :foo) [1 2 3])]
          (t/is (= [:c :b] res))
          (t/is (= [1 2 :a 100 138 :foo :b] stack))))
      (t/testing "Tail Call Optimization"
        (letfn [(f1 [t]
                  (if (< t 30000)
                    (do>
                     (st-push [t :a])
                     (st-push [t :b])
                     (st-push [t :c])
                     (st-push [t :d])
                     (tc (f2 t)))
                    (->state t)))
                (f2 [t]
                    (do>
                     (st-push :qqq)
                     (f1 (inc t))))]
          (let [[res stack] ((f1 0) [])
                len (count stack)]
            (t/is (= 30000 res))
            (t/is (= 150000 len))
            (t/is (= [:qqq [29999 :a] [29999 :b] [29999 :c] [29999 :d] :qqq]
                     (subvec stack (- len 6)))))))))
  (t/testing "Cont"
    (t/testing "Base"
      (let [ct-m
            (>>=
             (Cont. (fn [c] (concat (c [1]) (c [80]))))
             (fn [t1]
               (>>=
                (->cont (conj t1 2))
                (fn [t2]
                  (>>=
                   (->cont (conj t2 7))
                   (fn [t3]
                     (->cont (concat t3 t2))))))))]
        (t/is (= [1 2 7 1 2 80 2 7 80 2] (ct-m identity)))))
    (t/testing "Do block") ;TODO
    (t/testing "Tail Call Optimization") ;TODO
    (t/testing "Suspended computation"))) ;TODO
