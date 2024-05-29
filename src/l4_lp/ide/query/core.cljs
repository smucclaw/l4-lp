(ns l4-lp.ide.query.core 
  (:require [applied-science.js-interop :as jsi]
            [lambdaisland.uri :as uri]
            [meander.epsilon :as m]))

(def ^:private swipl-prelude-qlf-url
  (let [app-url (jsi/get-in js/window [:location :origin])]
    (str (uri/join app-url "./resources/swipl/prelude.qlf"))))

(def ^:private no-op
  (constantly nil))

(def worker-js-url
  "/js/l4_ide/query_worker.js")

(defn transpile-and-query-on-worker!
  [& {:keys [l4-program worker
             on-transpiled-prolog on-query-result on-done]
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
      (jsi/call worker :postMessage
                #js {:swipl-prelude-qlf-url swipl-prelude-qlf-url
                     :l4-program l4-program}))))