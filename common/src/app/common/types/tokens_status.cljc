;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.common.types.tokens-status
  (:require
   #?(:clj [app.common.fressian :as fres])
   #?(:clj [clojure.data.json :as c.json])
   [app.common.schema :as sm]
   [app.common.schema.generators :as sg]
   [app.common.transit :as t]
   [app.common.types.tokens-lib :as ctob]
   [clojure.core.protocols :as cp]
   [clojure.datafy :refer [datafy]]
   [clojure.pprint :as pp]))

;; TokensStatus datatype contains the status of the active themes and sets
;; in a tokens library.

(defprotocol ITokensStatus
  (activate-theme [_ theme-id] "Add a theme uuid to active themes")
  (deactivate-theme [_ theme-id] "Remove a theme uuid from active themes")
  (toggle-theme-active [_ theme-id] "Toggle a theme uuid in active themes")
  (theme-active? [_ theme-id] "Check if a theme uuid is active")
  (active-theme-count [_] "Return the number of active themes")
  (activate-set [_ set-id] "Add a set uuid to active sets")
  (deactivate-set [_ set-id] "Remove a set uuid from active sets")
  (toggle-set-active [_ set-id] "Toggle a set uuid in active sets")
  (set-active? [_ set-id] "Check if a set uuid is active")
  (active-set-count [_] "Return the number of active sets"))

(deftype TokensStatus [active-themes active-sets]
  cp/Datafiable
  (datafy [_]
    {:active-themes active-themes
     :active-sets active-sets})

  #?@(:clj
      [c.json/JSONWriter
       (-write [this writter options]
               (c.json/-write (datafy this) writter options))])

  ITokensStatus
  (activate-theme [_ theme-id]
    (TokensStatus. (conj active-themes theme-id) active-sets))

  (deactivate-theme [_ theme-id]
    (TokensStatus. (disj active-themes theme-id) active-sets))

  (toggle-theme-active [this theme-id]
    (if (contains? active-themes theme-id)
      (deactivate-theme this theme-id)
      (activate-theme this theme-id)))

  (theme-active? [_ theme-id]
    (contains? active-themes theme-id))

  (active-theme-count [_]
    (prn active-themes)
    (count active-themes))

  (activate-set [_ set-id]
    (TokensStatus. active-themes (conj active-sets set-id)))

  (deactivate-set [_ set-id]
    (TokensStatus. active-themes (disj active-sets set-id)))

  (toggle-set-active [this set-id]
    (if (contains? active-sets set-id)
      (deactivate-set this set-id)
      (activate-set this set-id)))

  (set-active? [_ set-id]
    (contains? active-sets set-id))
  
  (active-set-count [_]
    (count active-sets)))

;; === Helper & Predicate ===

(defn map->TokensStatus
  [{:keys [active-themes active-sets]}]
  (TokensStatus. active-themes active-sets))

(defn tokens-status?
  [o]
  (instance? TokensStatus o))

;; === Schemas, Check functions & Constructor ===

(declare make-tokens-status)

(def schema:tokens-status-attrs
  [:map {:title "TokensStatus"}
   [:active-themes [:set {:gen/max 5} ::sm/uuid]]
   [:active-sets [:set {:gen/max 5} ::sm/uuid]]])

(def schema:tokens-status
  [:and {:gen/gen (->> (sg/generator schema:tokens-status-attrs)
                       (sg/fmap #(make-tokens-status %)))}
   [:fn tokens-status?]])

(def ^:private check-tokens-status-attrs
  (sm/check-fn schema:tokens-status-attrs
               :hint "expected valid params for tokens-status"))

(def check-tokens-status
  (sm/check-fn schema:tokens-status
               :hint "expected valid tokens-status"))

(defn make-tokens-status
  [& {:as attrs}]
  (-> attrs
      (update :active-themes #(or % #{}))
      (update :active-sets #(or % #{}))
      (check-tokens-status-attrs)
      (map->TokensStatus)))

(defn make-tokens-status-from-lib
  "Make a TokensStatus from a TokensLib, activating the themes and sets
   marked as active in the library (to migrate from legacy files)."
  [tokens-lib]
  (let [active-themes (into #{}
                            (comp (map :id)
                                  (filter #(not= % ctob/hidden-theme-id)))
                            (ctob/get-active-themes tokens-lib))
        active-sets   (into #{}
                            (comp (map #(ctob/get-set-by-name tokens-lib %))
                                  (map ctob/get-id))
                            (ctob/get-active-themes-set-names tokens-lib))]
    (make-tokens-status :active-themes active-themes
                        :active-sets active-sets)))

;; === Pretty-print for debugging ===

(defmethod pp/simple-dispatch TokensStatus [^TokensStatus obj]
  (.write *out* "#penpot/tokens-status ")
  (pp/pprint-newline :miser)
  (pp/pprint (datafy obj)))

#?(:clj
   (do
     (defmethod print-method TokensStatus
       [^TokensStatus this ^java.io.Writer w]
       (.write w "#penpot/tokens-status ")
       (print-method (datafy this) w))

     (defmethod print-dup TokensStatus
       [^TokensStatus this ^java.io.Writer w]
       (print-method this w)))

   :cljs
   (extend-type TokensStatus
     cljs.core/IPrintWithWriter
     (-pr-writer [this writer opts]
       (-write writer "#penpot/tokens-status ")
       (-pr-writer (datafy this) writer opts))

     cljs.core/IEncodeJS
     (-clj->js [this]
       (clj->js (datafy this)))))

;; === Transit serialization ===

(t/add-handlers!
 {:id "penpot/tokens-status"
  :class TokensStatus
  :wfn datafy
  :rfn #(make-tokens-status %)})

;; === Fressian serialization ===

#?(:clj
   (fres/add-handlers!
    {:name "penpot/tokens-status/v1"
     :class TokensStatus
     :wfn (fn [n w o]
            (fres/write-tag! w n 1)
            (fres/write-object! w (datafy o)))
     :rfn (fn [r]
            (let [obj (fres/read-object! r)]
              (make-tokens-status obj)))}))
