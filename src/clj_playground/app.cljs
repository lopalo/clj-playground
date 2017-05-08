
(ns clj-playground.app
  (:require [clojure.string :as s]
            [rum.core :as r]
            [clj-playground.helpers :refer [cursor-collection
                                            expand-tags]]))

(declare
 item-list
 list-item
 tabs
 control
 swap-items!
 delete-items!)

(def records-per-page 10)

(r/defc app < r/reactive
  [*state]
  (let [*header (r/cursor-in *state [:header])
        *list (r/cursor-in *state [:list])]
    (expand-tags
     [:.ui.centered.grid
      [:.row>.eight.wide.column
       :.ui.raised.segment>h2.ui.header
       (r/react *header)]
      (tabs *list)
      (control *state)])))

(r/defcs item-list < r/reactive
                     (r/local 0 ::page-num)
  [state *list tab]
  (let [*page-num (::page-num state)
        predicate (case tab
                    :all (constantly true)
                    :completed :completed
                    :active (complement :completed))
        records (cursor-collection *list predicate)
        pages (partition-all records-per-page records)
        page-count (count pages)
        page-num (min @*page-num (dec page-count))
        page-records (if (seq pages) (nth pages page-num) ())
        fst-id (-> @*list first :id)
        lst-id (-> @*list last :id)
        movable (= tab :all)]
    [:div
     [:.ui.relaxed.divided.list
      (for [[idx *record] page-records :let [id (:id @*record)]]
        (list-item *record
                   {:move-up! (when (and movable (not= id fst-id))
                                #(swap-items! *list idx (dec idx)))
                    :move-down! (when (and movable (not= id lst-id))
                                  #(swap-items! *list idx (inc idx)))
                    :delete! (fn []
                               (delete-items! *list #(= (:id %) id)))}))]
     (when (> page-count 1)
       [:.ui.pagination.mini.menu
        (for [p-num (range page-count)
              :let [active (= p-num page-num)]]
          [:a.item
           {:class (if active "active" "")
            :on-click (fn [_] (reset! *page-num p-num))}
           (inc p-num)])])]))

(r/defcs list-item < r/reactive
                     (r/local false ::editing)
                     {:key-fn #(:id @%)}
  [state *record params]
  (let [{:keys [user completed description]} (r/react *record)
        {:keys [move-up! move-down! delete!]} params
        *editing (::editing state)]
    [:.item
     [:i.right.floated.large.action.orange.trash.outline.icon
      {:on-click (fn [_] (delete!))}]
     [:i.right.floated.large.action.icon
      {:class (if completed
                ["checkmark" "box"]
                ["square" "outline"])
       :on-click (fn [_] (swap! *record update :completed not))}]
     (when move-up!
       [:i.right.floated.large.action.arrow.up.icon
        {:on-click (fn [_] (move-up!))}])
     (when move-down!
       [:i.right.floated.large.action.arrow.down.icon
        {:on-click (fn [_] (move-down!))}])
     [:.bottom.aligned.content
      [:.header user]
      [:.description
       (if @*editing
         [:.ui.small.action.input
          [:input.ui.input
           {:value description
            :on-change #(swap!
                         *record
                         assoc
                         :description
                         (-> % .-target .-value))}]
          [:.button.ui.small.icon.button
           {:on-click (fn [_]
                        (swap! *record update :description s/trim)
                        (reset! *editing false))}
           [:i.checkmark.icon]]]
         [:div
          description
          \space
          [:i.edit.large.action.icon
           {:on-click (fn [_] (reset! *editing true))}]])]]]))

(r/defcs tabs < (r/local :all ::tab)
  [state *list]
  (letfn [(active-cls [attrs tab]
            (if (= tab @(state ::tab)) (assoc attrs :class :active) attrs))
          (menu-item [tab title]
                     [:.item
                      (active-cls {:on-click
                                   (fn [_] (reset! (state ::tab) tab))}
                                  tab)
                      title])
          (tab-content [tab content]
                       [:.ui.bottom.attached.tab.segment
                        (active-cls {} tab)
                        content])
          (tab-list [tab] (tab-content tab (item-list *list tab)))]
    (expand-tags
     [:.row>.twelve.wide.column
      :.ui.tall.stacked.left.aligned.segment
      [:.ui.top.attached.tabular.menu
       (menu-item :all "All")
       (menu-item :active "Active")
       (menu-item :completed "Completed")]
      (tab-list :all)
      (tab-list :active)
      (tab-list :completed)])))

(r/defcs control < r/reactive
                   (r/local nil ::user)
                   (r/local "" ::description)
  [state *state]
  (let [*next-id (r/cursor-in *state [:next-id])
        *list (r/cursor-in *state [:list])
        *users (r/cursor-in *state [:users])
        *description (state ::description)
        *user (state ::user)
        users (r/react *users)
        completed (->> *list r/react (filter :completed) count)
        can-add (-> *description deref s/blank? not)
        add-record!
        (fn [_]
          (swap! *list conj {:id @*next-id
                             :user (or @*user (first users))
                             :description (s/trim @*description)
                             :completed false})
          (swap! *next-id inc)
          (reset! *user nil)
          (reset! *description ""))]
    (expand-tags
     [:.row>.twelve.wide.column
      :.ui.left.aligned.segment
      :.ui.form>.fields
      [:.field>.ui.action.input
       [:input.ui.input
        {:value @*description
         :on-change #(reset! *description (-> % .-target .-value))}]
       [:select.ui.dropdown
        {:value @*user
         :on-change #(reset! *user (-> % .-target .-value))}
        (for [usr users]
          [:option {:value usr} usr])]
       [:.button.ui.icon.button
        {:class (if can-add "" :disabled)
         :on-click add-record!}
        [:i.plus.icon]]]
      [:.field>button.ui.orange.basic.button
       {:on-click (fn [_] (delete-items! *list :completed))
        :class (if (> completed 0) "" :disabled)}
       "Delete completed: "
       completed]])))

(defn swap-items!
  [*list index index']
  (let [swap
        #(let [item (nth % index)
               item' (nth % index')]
           (-> % (assoc index item') (assoc index' item)))]
    (swap! *list swap)))

(defn delete-items!
  [*list pred]
  (swap! *list #(filterv (complement pred) %)))
