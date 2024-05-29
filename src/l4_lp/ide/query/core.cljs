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

;; This tells the web worker to load the swipl prelude qlf file from a URL
;; relative to that of the parent web app.
;; This is needed as the web worker does not know of the parent web app's URL,
;; and does not have access to its js/window object.
;; The only way to communicate this info from the parent app to the worker is
;; to post it over as a message, via the web worker API.
(post-data-as-js! :worker worker
                  :tag "swipl-prelude-qlf-url"
                  :payload swipl-prelude-qlf-url)

(def ^:private no-op
  (constantly nil))

(defn transpile-and-query!
  [l4-program
   & {:keys [on-transpiled-prolog on-query-result on-done]
      :or {on-transpiled-prolog no-op
           on-query-result no-op
           on-done no-op}}]
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

    (when-not (jsi/get worker :onmessage)
      (jsi/assoc! worker :onmessage on-message!)
      (post-data-as-js! :worker worker
                        :tag "l4-program"
                        :payload l4-program))))