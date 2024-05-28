(ns l4-lp.ide.query.core 
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.ide.query.utils :refer [post-data-as-js!]]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]))

(def ^:private swipl-prelude-qlf-url
  (let [app-url (jsi/get-in js/window [:location :origin])]
    (str (uri/join app-url "./resources/swipl/prelude.qlf"))))

(def ^:private worker
  (new js/Worker "/js/l4_ide/query_worker.js"
       #js {:type "module"}))

(post-data-as-js! :worker worker
                  :tag "swipl-prelude-qlf-url"
                  :payload swipl-prelude-qlf-url)

(defn transpile-and-query!
  [l4-program
   & {:keys [on-transpiled-prolog on-query-result on-done]}]
  (let [on-message!
        (jsi/fn [^:js {:keys [data]}]
          (m/match data
            #js {:tag (m/some "transpiled-prolog")
                 :payload (m/some ?transpiled-prolog)}
            (on-transpiled-prolog ?transpiled-prolog)

            #js {:tag (m/some "query-result")
                 :payload (m/some ?query-result)}
            (on-query-result ?query-result)

            #js {:tag (m/some "done")}
            (do (jsi/assoc! worker :onmessage nil)
                (on-done))))]

    (jsi/update! worker :onmessage
                 (some-fn identity (constantly on-message!)))

    (post-data-as-js! :worker worker
                      :tag "l4-program"
                      :payload l4-program)))