(ns l4-lp.ide.query.core 
  (:require [applied-science.js-interop :as jsi]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]))

(def ^:private query-worker
  (new js/Worker "/js/l4_ide/query_worker.js"
       #js {:type "module"}))

(def ^:private swipl-prelude-qlf-url
  (let [app-url (jsi/get-in js/window [:location :origin])]
    (str (uri/join app-url "./resources/swipl/prelude.qlf"))))

(def ^:private query-running?
  (atom false))

(defn transpile-and-query!
  [l4-program
   & {:keys [on-transpiled-prolog on-query-result on-done]}]
  (when-not @query-running?
    (reset! query-running? true)

    (jsi/assoc!
     query-worker :onmessage
     (jsi/fn [^:js {:keys [data]}]
       (m/match data
         #js {:tag (m/some "transpiled-prolog")
              :payload (m/some ?transpiled-prolog)}
         (on-transpiled-prolog ?transpiled-prolog)

         #js {:tag (m/some "query-result")
              :payload (m/some ?query-result)}
         (on-query-result ?query-result)

         #js {:tag (m/some "done")}
         (do (reset! query-running? false)
             (on-done)))))

    (jsi/call query-worker :postMessage
              #js {:l4-program l4-program
                   :swipl-prelude-qlf-url swipl-prelude-qlf-url})))