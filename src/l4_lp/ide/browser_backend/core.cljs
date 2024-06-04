(ns l4-lp.ide.browser-backend.core 
  (:require [applied-science.js-interop :as jsi]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]))

(def ^:private swipl-prelude-qlf-url
  (let [app-url (jsi/get-in js/window [:location :href])]
    (str (uri/join app-url "./resources/swipl/prelude.qlf"))))

(def ^:private no-op
  (constantly nil))

(def worker-js-url
  "./js/l4_ide/worker.js")

(def swipl-prelude-url-data
  #js {:tag "swipl-prelude-qlf-url"
       :payload swipl-prelude-qlf-url})

(defn l4-program->worker-data [l4-program]
  #js {:tag "l4-program"
       :payload l4-program})

(defn on-data-from-worker
  [data
   & {:keys [on-transpiled-prolog on-query-result on-done]
      :or {on-transpiled-prolog no-op
           on-query-result no-op
           on-done no-op}}]
  (m/match data
    #js {:tag (m/some "transpiled-prolog")
         :payload (m/some ?transpiled-prolog)}
    (on-transpiled-prolog ?transpiled-prolog)

    #js {:tag (m/some "query-result")
         :payload (m/some ?query-result)}
    (on-query-result ?query-result)

    nil (on-done)))