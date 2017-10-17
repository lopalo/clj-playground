(ns clj-playground.table
  (:require [clojure.test :as t]))

(defprotocol Table
  (insert [this record])
  (delete [this id])
  (get-by [this field value])
  (get-range-by
    [this field test value]
    [this field test value reverse?]
    [this field start-test start-val end-test end-val]
    [this field start-test start-val end-test end-val reverse?])
  (delete-by [this field value])
  (delete-range-by
    [this field test value]
    [this field start-test start-val end-test end-val])
  (update-by [this field value f])
  (update-range-by
    [this field test value f]
    [this field start-test start-val end-test end-val f])
  (-internals [this]))

(defrecord T [fields-meta pk idx])

(defn- reduce-idx [idx f fields]
  (reduce (fn [idx field]
            (update idx field f field))
          idx
          fields))

(defn- table* [{:keys [fields-meta pk idx] :as t}]
  (let [records (idx pk)
        unique? (fn [field] (-> field fields-meta :unique))
        range-ids
        (fn [field pairs]
          (let [ids (map second pairs)]
            (if (unique? field)
              ids
              (->> ids (map seq) flatten))))
        update-records
        (fn [this ids f]
          (let [updated-records (map (comp f records) ids)]
            (as-> this $
                  (reduce delete $ ids)
                  (reduce insert $ updated-records))))]
    (reify
      Object
      (toString [this] (str "Table: " records))
      Table
      (insert [this record]
        (table*
         (let [id (pk record)
               index-field
               (fn [f-idx field]
                 (let [value (get record field)
                       f-meta (fields-meta field)
                       is-unique (:unique f-meta)]
                   (when is-unique
                     (assert (not (f-idx value)) (str field " duplicate")))
                   (if is-unique
                     (assoc f-idx value (if (= field pk) record id))
                     (update f-idx value (fnil conj #{}) id))))]
           (assoc t :idx (reduce-idx idx index-field (keys fields-meta))))))
      (delete [this id]
        (table*
         (let [record (records id)
               del-field
               (fn [f-idx field]
                 (let [value (get record field)
                       id-set (f-idx value)]
                   (if (or (unique? field)
                           (and (= (count id-set) 1) (id-set id)))
                     (dissoc f-idx value)
                     (update f-idx value disj id))))]
           (if record
             (assoc t :idx (reduce-idx idx del-field (keys fields-meta)))
             t))))
      (get-by [this field value]
        (let [ids (get-in idx [field value])]
          (if (unique? field)
            (records ids)
            (map records ids))))
      (get-range-by [this field test value]
        (get-range-by this field test value false))
      (get-range-by [this field test value reverse?]
        (let [selector (if reverse? rsubseq subseq)
              ids (range-ids field (selector (idx field) test value))]
          (map records ids)))
      (get-range-by [this field start-test start-val end-test end-val]
        (get-range-by this field start-test start-val end-test end-val false))
      (get-range-by [this field start-test start-val end-test end-val reverse?]
        (let [selector (if reverse? rsubseq subseq)
              ids (range-ids field (selector (idx field)
                                             start-test
                                             start-val
                                             end-test
                                             end-val))]
          (map records ids)))
      (delete-by [this field value]
        (let [ids (get-in idx [field value])]
          (if (unique? field)
            (delete this ids)
            (reduce delete this ids))))
      (delete-range-by [this field test value]
        (let [ids (range-ids field (subseq (idx field) test value))]
          (reduce delete this ids)))
      (delete-range-by [this field start-test start-val end-test end-val]
        (let [ids (range-ids field (subseq (idx field)
                                           start-test
                                           start-val
                                           end-test
                                           end-val))]
          (reduce delete this ids)))
      (update-by [this field value f]
        (let [ids (get-in idx [field value])]
          (update-records this (if (unique? field) [ids] ids) f)))
      (update-range-by [this field test value f]
        (let [ids (range-ids field (subseq (idx field) test value))]
          (update-records this ids f)))
      (update-range-by [this field start-test start-val end-test end-val f]
        (let [ids (range-ids field (subseq (idx field)
                                           start-test
                                           start-val
                                           end-test
                                           end-val))]
          (update-records this ids f)))
      (-internals [this] t))))

(def ^:private flags #{:unique :sorted})

(defn- field-meta [field-flags]
  (if (sequential? field-flags)
    (let [[field & field-flags] field-flags]
      (doseq [flag field-flags]
        (assert (flags flag) flags))
      (merge
       {:field field}
       (zipmap flags (repeat false))
       (zipmap field-flags (repeat true))))
    (merge
     {:field field-flags}
     (zipmap flags (repeat false)))))

(defn table [fields-flags records]
  (let [meta-vec (assoc-in (mapv field-meta fields-flags)
                           [0 :unique]
                           true)
        pk (get-in meta-vec [0 :field])
        fields-meta (reduce #(assoc %1 (:field %2) %2) {} meta-vec)
        idx (reduce-kv #(assoc %1 %2 (if (:sorted %3)
                                       (sorted-map)
                                       (hash-map)))
                       {}
                       fields-meta)]
    (reduce insert (table* (T. fields-meta pk idx)) records)))

(t/deftest base-test
  (let [tbl (table [:id
                    [:foo :unique :sorted]
                    [:bar :unique]
                    'baz
                    [:qux :sorted]]
                   [{:id 71 :foo 12 :bar :a 'baz [1] :qux "BA"}
                    {:id 82 :foo 4 :bar :c 'baz [2] :qux "AB"}
                    {:id 84 :foo 16 :bar :b 'baz [1] :qux "BA" :ff {:a 3}}
                    {:id 85 :foo 29 :bar :d 'baz [3] :qux "BB"}])
        tbl1 (-> tbl
                 (insert {:id 91 :foo 45 :bar :cc 'baz [3] :qux "AB"})
                 (insert {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"})
                 (insert {:id 93 :foo 171 :bar :ca 'baz [1] :qux "BB"}))
        tbl2 (-> tbl1
                 (delete 91)
                 (delete 666))
        tbl3 (delete-by tbl1 :bar :c)
        tbl4 (delete-by tbl1 'baz [1])
        tbl5 (delete-range-by tbl1 :qux > "AB" <= "BB")
        tbl6 (delete-range-by tbl1 :foo >= 4 <= 100)
        tbl7 (-> tbl1
                 (update-range-by :foo >= 4 #(update % :foo inc))
                 (update-range-by :qux < "CC" #(update % 'baz assoc 0 777)))]
    (t/is
     (=
      (T. {:bar {:field :bar :sorted false :unique true}
           :foo {:field :foo :sorted true :unique true}
           :id {:field :id :sorted false :unique true}
           :qux {:field :qux :sorted true :unique false}
           'baz {:field 'baz :sorted false :unique false}}
          :id
          {:bar {:a 71 :b 84 :c 82 :d 85}
           :foo {4 82 12 71 16 84 29 85}
           :id {71 {:bar :a :foo 12 :id 71 :qux "BA" 'baz [1]}
                82 {:bar :c :foo 4 :id 82 :qux "AB" 'baz [2]}
                84 {:bar :b :foo 16 :id 84 :qux "BA" 'baz [1] :ff {:a 3}}
                85 {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}}
           :qux {"AB" #{82} "BA" #{71 84} "BB" #{85}}
           'baz {[1] #{71 84} [2] #{82} [3] #{85}}})
      (-internals tbl)))

    (t/is (thrown? AssertionError (table [[:ff :not-unique]] [])))
    (t/is (thrown? AssertionError
                   (insert tbl {:id 840 :foo 16 :bar :qq 'baz [] :qux "AB"})))

    (t/is
     (=
      {:bar {:a 71 :b 84 :c 82 :d 85 :cc 91 :cb 92 :ca 93}
       :foo {4 82 12 71 16 84 29 85 45 91 170 92 171 93}
       :id {71 {:bar :a :foo 12 :id 71 :qux "BA" 'baz [1]}
            82 {:bar :c :foo 4 :id 82 :qux "AB" 'baz [2]}
            84 {:bar :b :foo 16 :id 84 :qux "BA" 'baz [1] :ff {:a 3}}
            85 {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}
            91 {:id 91 :foo 45 :bar :cc 'baz [3] :qux "AB"}
            92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}
            93 {:id 93 :foo 171 :bar :ca 'baz [1] :qux "BB"}}
       :qux {"AB" #{82 91} "BA" #{71 84} "BB" #{85 93} "CC" #{92}}
       'baz {[1] #{71 84 93} [2] #{82} [3] #{85 92 91}}}
      (-> tbl1 -internals :idx)))

    (t/is
     (=
      {:bar {:a 71 :b 84 :c 82 :d 85 :cb 92 :ca 93}
       :foo {4 82 12 71 16 84 29 85 170 92 171 93}
       :id {71 {:bar :a :foo 12 :id 71 :qux "BA" 'baz [1]}
            82 {:bar :c :foo 4 :id 82 :qux "AB" 'baz [2]}
            84 {:bar :b :foo 16 :id 84 :qux "BA" 'baz [1] :ff {:a 3}}
            85 {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}
            92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}
            93 {:id 93 :foo 171 :bar :ca 'baz [1] :qux "BB"}}
       :qux {"AB" #{82} "BA" #{71 84} "BB" #{85 93} "CC" #{92}}
       'baz {[1] #{71 84 93} [2] #{82} [3] #{85 92}}}
      (-> tbl2 -internals :idx)))

    (t/is
     (=
      {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}
      (get-by tbl2 :bar :d)))

    (t/is
     (=
      '({:id 93 :foo 171 :bar :ca baz [1] :qux "BB"}
        {:bar :a :foo 12 :id 71 :qux "BA" baz [1]}
        {:bar :b :foo 16 :id 84 :qux "BA" baz [1] :ff {:a 3}})
      (get-by tbl1 'baz [1])))

    (t/is
     (=
      '({:bar :cb :foo 170 :id 92 :qux "CC" baz [3]}
        {:bar :d :foo 29 :id 85 :qux "BB" baz [3]}
        {:bar :b :foo 16 :id 84 :qux "BA" baz [1] :ff {:a 3}}
        {:bar :a :foo 12 :id 71 :qux "BA" baz [1]}
        {:bar :c :foo 4 :id 82 :qux "AB" baz [2]})
      (get-range-by tbl2 :foo  >= 4 <= 170 true)))

    (t/is
     (=
      '({:bar :a :foo 12 :id 71 :qux "BA" baz [1]}
        {:bar :b :foo 16 :id 84 :qux "BA" baz [1] :ff {:a 3}}
        {:bar :d :foo 29 :id 85 :qux "BB" baz [3]}
        {:bar :cc :foo 45 :id 91 :qux "AB" baz [3]}
        {:bar :cb :foo 170 :id 92 :qux "CC" baz [3]}
        {:bar :ca :foo 171 :id 93 :qux "BB" baz [1]})
      (get-range-by tbl1 :foo > 4)))

    (t/is
     (=
      {:bar {:a 71 :b 84 :d 85 :cc 91 :cb 92 :ca 93}
       :foo {12 71 16 84 29 85 45 91 170 92 171 93}
       :id {71 {:bar :a :foo 12 :id 71 :qux "BA" 'baz [1]}
            84 {:bar :b :foo 16 :id 84 :qux "BA" 'baz [1] :ff {:a 3}}
            85 {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}
            91 {:id 91 :foo 45 :bar :cc 'baz [3] :qux "AB"}
            92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}
            93 {:id 93 :foo 171 :bar :ca 'baz [1] :qux "BB"}}
       :qux {"AB" #{91} "BA" #{71 84} "BB" #{85 93} "CC" #{92}}
       'baz {[1] #{71 84 93} [3] #{85 92 91}}}
      (-> tbl3 -internals :idx)))

    (t/is
     (=
      {:bar {:c 82 :d 85 :cc 91 :cb 92}
       :foo {4 82 29 85 45 91 170 92}
       :id {82 {:bar :c :foo 4 :id 82 :qux "AB" 'baz [2]}
            85 {:bar :d :foo 29 :id 85 :qux "BB" 'baz [3]}
            91 {:id 91 :foo 45 :bar :cc 'baz [3] :qux "AB"}
            92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}}
       :qux {"AB" #{82 91} "BB" #{85} "CC" #{92}}
       'baz {[2] #{82} [3] #{85 92 91}}}
      (-> tbl4 -internals :idx)))

    (t/is
     (=
      {:bar {:c 82 :cc 91 :cb 92}
       :foo {4 82 45 91 170 92}
       :id {82 {:bar :c :foo 4 :id 82 :qux "AB" 'baz [2]}
            91 {:id 91 :foo 45 :bar :cc 'baz [3] :qux "AB"}
            92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}}
       :qux {"AB" #{82 91} "CC" #{92}}
       'baz {[2] #{82} [3] #{92 91}}}
      (-> tbl5 -internals :idx)))

    (t/is
     (=
      {:bar {:cb 92 :ca 93}
       :foo {170 92 171 93}
       :id {92 {:id 92 :foo 170 :bar :cb 'baz [3] :qux "CC"}
            93 {:id 93 :foo 171 :bar :ca 'baz [1] :qux "BB"}}
       :qux {"BB" #{93} "CC" #{92}}
       'baz {[1] #{93} [3] #{92}}}
      (-> tbl6 -internals :idx)))

    (t/is
     (=
      {:bar {:a 71 :b 84 :c 82 :d 85 :cc 91 :cb 92 :ca 93}
       :foo {5 82 13 71 17 84 30 85 46 91 171 92 172 93}
       :id {71 {:bar :a :foo 13 :id 71 :qux "BA" 'baz [777]}
            82 {:bar :c :foo 5 :id 82 :qux "AB" 'baz [777]}
            84 {:bar :b :foo 17 :id 84 :qux "BA" 'baz [777] :ff {:a 3}}
            85 {:bar :d :foo 30 :id 85 :qux "BB" 'baz [777]}
            91 {:id 91 :foo 46 :bar :cc 'baz [777] :qux "AB"}
            92 {:id 92 :foo 171 :bar :cb 'baz [3] :qux "CC"}
            93 {:id 93 :foo 172 :bar :ca 'baz [777] :qux "BB"}}
       :qux {"AB" #{82 91} "BA" #{71 84} "BB" #{85 93} "CC" #{92}}
       'baz {[3] #{92} [777] #{71 84 93 82 85 91}}}
      (-> tbl7 -internals :idx)))))
