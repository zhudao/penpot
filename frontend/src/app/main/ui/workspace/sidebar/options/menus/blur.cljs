;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.blur
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.main.data.workspace :as udw]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def blur-attrs [:blur :background-blur])

(defn create-blur [type]
  (let [id (uuid/next)]
    {:id id
     :type type
     :value 4
     :hidden false}))

(mf/defc blur-menu-content*
  [{:keys [blur-key value change-fn blur-values]}]
  (let [render-wasm?   (features/use-feature "render-wasm/v1")
        bg-blur?       (and render-wasm?
                            (contains? cf/flags :background-blur))
        is-hidden (get value :hidden)
        show-more-options*         (mf/use-state false)
        show-more-options (deref show-more-options*)
        toggle-more-options (mf/use-fn #(swap! show-more-options* not))

        handle-delete
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn []
           (change-fn #(dissoc % blur-key))))

        handle-toggle-visibility
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn []
           (change-fn #(update-in % [blur-key :hidden] not))))

        handle-change
        (mf/use-fn
         (mf/deps change-fn blur-key)
         (fn [value]
           (change-fn #(assoc-in % [blur-key :value] value))))

        handle-type-change
        (mf/use-fn
         (mf/deps change-fn value)
         (fn [type]
           (let [old-value value
                 new-value (assoc old-value :type (keyword type))]
             (change-fn
              #(cond
                 ;; llega background-blur y existe blur -> sustituir
                 (and (= type "background-blur")
                      (contains? % :blur))
                 (-> %
                     (dissoc :blur)
                     (assoc :background-blur new-value))

                 ;; llega layer-blur y existe background-blur -> sustituir
                 (and (= type "layer-blur")
                      (contains? % :background-blur))
                 (-> %
                     (dissoc :background-blur)
                     (assoc :blur new-value))

                 ;; ya existe el tipo correcto -> no hacer nada
                 (or (and (= type "background-blur")
                          (contains? % :background-blur))
                     (and (= type "layer-blur")
                          (contains? % :blur)))
                 %

                 ;; fallback: crear el blur correspondiente
                 (= value "background-blur")
                 (assoc % :background-blur (create-blur :background-blur))

                 :else
                 (assoc % :blur (create-blur :layer-blur)))))))

        bb-disabled? (and (= 2 (count blur-values))
                          (not= blur-key :background-blur))
        lb-disabled? (and (= 2 (count blur-values))
                          (not= blur-key :blur))

        type-options
        [{:value "layer-blur"  :disabled lb-disabled? :id "layer-blur" :label (tr "workspace.options.blur-options.layer-blur")}
         {:value "background-blur" :disabled bb-disabled?  :id "background-blur" :label (tr "workspace.options.blur-options.background-blur")}]]

    [:*
     [:div {:class (stl/css-case :first-row true
                                 :hidden is-hidden)}
      [:div {:class (stl/css :blur-info)
             :data-testid "blur-info"}
       (when bg-blur?
         [:> icon-button* {:class (stl/css-case :show-more true
                                                :selected show-more-options)
                           :on-click toggle-more-options
                           ;;TODO_BLUR: Add translation
                           :aria-label (tr "workspace.options.blur-options.toggle-more-options")
                           :icon i/menu}])
       (if bg-blur?
         [:> select* {:class (stl/css :blur-type-select)
                      :default-selected (d/name (:type value))
                      :options type-options
                      ;;TODO_BLUR: review this
                      :disabled (case is-hidden
                                  :multiple false
                                  nil false
                                  true true
                                  false false)
                      :on-change handle-type-change}]
         [:span {:class (stl/css :label)}
          (d/name (:type value))])]

      [:div {:class (stl/css :actions)}
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.options.blur-options.toggle-blur")
                         :on-click handle-toggle-visibility
                         :icon (if is-hidden i/hide i/shown)}]
       [:> icon-button* {:variant "ghost"
                         :aria-label (tr "workspace.options.blur-options.remove-blur")
                         :on-click handle-delete
                         :icon i/remove}]]]

     (when show-more-options
       [:div {:class (stl/css :second-row)}
        [:> numeric-input*
         {:class (stl/css :numeric-input)
          :placeholder "--"
          :min 0
          :text-icon "value"
          :on-change handle-change
          :name "blur-value"
          :value (:value value)}]])]))

(defn get-blurs [values]
  (cond-> []
    (:blur values)
    (conj {:key :blur
           :value (:blur values)})

    (:background-blur values)
    (conj {:key :background-blur
           :value (:background-blur values)})))

(mf/defc blur-menu* [{:keys [ids type values]}]
  (let [render-wasm?   (features/use-feature "render-wasm/v1")
        bg-blur?       (and render-wasm?
                            (contains? cf/flags :background-blur))

        blur-values          (get-blurs values)

        state*         (mf/use-state {:show-content true})
        state          (deref state*)
        open?          (:show-content state)

        toggle-content (mf/use-fn #(swap! state* update :show-content not))

        change!
        (mf/use-fn
         (mf/deps ids)
         (fn [update-fn]
           (st/emit! (udw/trigger-bounding-box-cloaking ids)
                     (dwsh/update-shapes ids update-fn))))

        handle-add
        (mf/use-fn
         (mf/deps change! blur-values)
         (fn []
           (cond
             (= 1 (count blur-values))
             (let [existing-key (:key (first blur-values))
                   new-key      (if (= existing-key :blur)
                                  :background-blur
                                  :blur)]
               (change! #(assoc % new-key (create-blur (if (= :blur new-key)
                                                         :layer-blur
                                                         :background-blur)))))
             (= 0 (count blur-values))
             (change! #(assoc % :blur (create-blur :layer-blur))))
             :else
             blur-values))]

    [:section {:class (stl/css :element-set)
               :hidden (not open?)
               :aria-label (tr "workspace.options.blur-effects-options.title")}
     (if bg-blur?
       [:*
        [:div {:class (stl/css :element-title)}
         [:> title-bar* {:collapsable  (some? blur-values)
                         :collapsed    (not open?)
                         :on-collapsed toggle-content
                         :aria-expanded open?
                         :aria-controls "blur-content"
                         :title        (cond
                                         ;; TODO_BLUR: Add translation
                                         (= type :multiple) (tr "multiple y background blur")
                                         (= type :group) (tr "group y background blur")
                                         :else (tr "workspace.options.blur-effects-options.title"))
                         :class        (stl/css-case :title-spacing-blur (not (some? blur-values)))}
          (when-not (< 1 (count blur-values))
            [:> icon-button*
             {:variant "ghost"
              :aria-label (tr "workspace.options.blur-options.add-blur")
              :on-click handle-add
              :icon i/add
              :data-testid "add-blur"}])]]

        (when (and open? (some? blur-values))
          [:div {:class (stl/css :element-set-content)
                 :hidden (not open?)
                 :id "blur-content"}

           (for [{:keys [key value]} blur-values]
             [:> blur-menu-content*
              {:key key
               :blur-key key
               :value value
               :blur-values blur-values
               :change-fn change!}])])]

       [:*
        [:div {:class (stl/css :element-title)}
         [:> title-bar* {:collapsable  (some? blur-values)
                         :collapsed    (not open?)
                         :on-collapsed toggle-content
                         :aria-expanded open?
                         :aria-controls "blur-content"
                         :title        (cond
                                         (= type :multiple) (tr "workspace.options.blur-options.title.multiple")
                                         (= type :group) (tr "workspace.options.blur-options.title.group")
                                         :else (tr "workspace.options.blur-options.title"))
                         :class        (stl/css-case :title-spacing-blur (not (some? blur-values)))}
          (when-not (> 1 (count blur-values))
            [:> icon-button*
             {:variant "ghost"
              :aria-label (tr "workspace.options.blur-options.add-blur")
              :on-click handle-add
              :icon i/add
              :data-testid "add-blur"}])]]

        (when (and open? (some? blur-values))
          [:div {:class (stl/css :element-set-content)
                 :hidden (not open?)
                 :id "blur-content"}
           [:> blur-menu-content* {:blur-key :blur
                                   :value (first blur-values)
                                   :blur-values blur-values
                                   :change-fn change!}]])])]))