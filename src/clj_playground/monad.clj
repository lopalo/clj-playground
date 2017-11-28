(ns clj-playground.monad
  (:require [clojure.test :as t]))

(set! *warn-on-reflection* true)

(defprotocol Monad
  (bind [this reaction])
  (tail-call [this]))

(defn >>= [monad reaction]
  (if (satisfies? Monad monad)
    (bind monad reaction)
    (reaction monad)))

(defmacro >> [monad monad']
  `(>>= ~monad (fn [~'_] ~monad')))

(defn >>' [monad monad']
  (>> monad monad'))

(defn =<< [reaction monad]
  (>>= monad reaction))

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

(def tco tail-call-loop)

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
             (computation
              (fn [value]
                (let [computation' (.computation ^Cont (reaction value))]
                  (computation' cont)))))))
  (tail-call [this]
    (Cont. (fn [cont]
             (TailCall. #(computation cont)))))
  clojure.lang.IFn
  (invoke [this cont]
    (-> cont computation tail-call-loop)))

(defn ->cont [value]
  (Cont. (fn [cont] (cont value))))

(defn call-cc [f]
  (Cont. (fn [cont]
           (let [computation
                 (.computation ^Cont (f (fn [value]
                                          (Cont. (fn [_] (cont value))))))]
             (computation cont)))))

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
          (t/is (= [1 2 :a 100 138 :foo :b] stack))))))

  (t/testing "Cont"
    (t/testing "Base"
      (let [ct-m
            (>>=
             (Cont. (fn [c] (into (c [1]) (c [80]))))
             (fn [t1]
               (>>=
                (->cont (conj t1 2))
                (fn [t2]
                  (>>=
                   (->cont (conj t2 7))
                   (fn [t3]
                     (->cont (into t3 t2))))))))]
        (t/is (= [1 2 7 1 2 80 2 7 80 2] (ct-m identity)))))
    (t/testing "Do block"
      (let [f (fn [d]
                (do>
                 (|> (Cont. (fn [c] (c [1]))) t1)
                 (|> (->cont (conj t1 2)) t2)
                 (|> (->cont (conj t2 d)) t3)
                 (->cont (into t3 t2))))]
        (t/is (= [1 2 77 1 2] ((f 77) identity)))))
    (t/testing "Call CC"
      (let [f (fn [call]
                (do>
                 (<| t1 (call-cc
                         (fn [c]
                           (do>
                            (when call (c 100))
                            (<| t2 (->cont 11))
                            (->cont (+ t2 200))))))
                 (->cont (+ t1 12))))]
        (t/is (= 112 ((f true) identity)))
        (t/is (= 223 ((f false) identity)))))
    (t/testing "Tail Call Optimization"
      (letfn [(add [n] (fn [c] (+ n)))
              (f1 [t]
                  (if (< t 5000000)
                    (do>
                     (<| a (->cont (+ t 2)))
                     (<| b (->cont (+ a 1)))
                     (tc (f2 b)))
                    (->cont t)))
              (f2 [t]
                  (do>
                   (<| a (->cont (+ t 8)))
                   (<| b (->cont (+ a 11)))
                   (f1 b)))]
        (let [ct-m (do>
                    (f2 0)
                    (Cont. (fn [c] (+ (/ (-> nil c tco) 2)
                                      (/ (-> nil c tco) 2)
                                      (/ (-> nil c tco) 2))))
                    (<| a (f1 0))
                    (<| b (->cont (+ a 13)))
                    (<| c (f2 b))
                    (->cont c))]
          (t/is (= 7500057 (ct-m identity))))))
    (t/testing "Suspended computation"
      (let [yield (Cont. identity)
            ct-m1
            (do>
             (<| a yield)
             (<| b yield)
             (->cont [a b :a]))
            ct-m2
            (do>
             (<| a yield)
             (<| b (tc ct-m1))
             (<| c yield)
             (->cont (conj b a c :b)))
            ct-m3
            (do>
             (<| a ct-m2)
             (<| b yield)
             (<| c yield)
             (->cont (conj a b c :c)))
            input (range 6)
            output (reduce #((tco %1) %2) (ct-m3 identity) input)]
        (t/is (= [1 2 :a 0 3 :b 4 5 :c] output))))))
