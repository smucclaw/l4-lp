(ns l4-lp.ide.browser-backend.core 
  (:require [applied-science.js-interop :as jsi]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]))

(def worker-js-url
  "./js/l4_ide/worker.js")

(def ^:private swipl-prelude-qlf-url
  (let [absolute-app-url (jsi/get-in js/window [:location :href])]
    (str (uri/join absolute-app-url
                   swipl-wasm-query/swipl-prelude-qlf-url))))

(def init-swipl-data
  #js {:tag "init-swipl-with-prelude-url"
       :payload swipl-prelude-qlf-url})

(defn l4-program->run-query-data [l4-program]
  #js {:tag "run-l4-query"
       :payload l4-program})

(def ^:private no-op
  (constantly nil))

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