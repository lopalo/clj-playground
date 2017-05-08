(ns clj-playground.app-state)

(def initial-state
  {:header "Task list"
   :users ["Voldemar" "Bob" "Sam" "Boris"]
   :next-id 3
   :list
   [{:id 0 :user "Bob" :completed false :description ""}
    {:id 1 :user "Sam" :completed false :description "Some decription"}
    {:id 2 :user "Boris" :completed false :description ""}]})

(defonce *root-state (atom initial-state))

(comment
  (reset! *root-state initial-state)
  (swap! *root-state assoc :header "Another list")
  (swap! *root-state assoc :header "Foo list")
  (swap! *root-state assoc-in [:list 1 :completed] true))

(identity @*root-state)
