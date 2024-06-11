(ns l4-lp.ide.browser-backend.worker
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [meander.epsilon :as m]
            [promesa.core :as prom]))

(def ^:private swipl
  (atom nil))

(defn- post-data-as-js! [& {:keys [tag payload]}]
  (let [js-data (bean/->js {:tag tag :payload payload})]
    (js/postMessage js-data)))

(defn- post-ready! []
  (js/postMessage nil))

(defn- transpile-and-query! [l4-program]
  (let [transpiled-prolog (-> l4-program
                              l4->prolog/l4->prolog-program+queries)]
    (post-data-as-js!
     :tag "transpiled-prolog" :payload transpiled-prolog)

    (-> transpiled-prolog
        (swipl-wasm-query/query-and-trace!
         :swipl @swipl
         :on-query-result
         #(post-data-as-js! :tag "query-result" :payload %)))))

(defn- on-message! [event]
  (m/match (jsi/get event :data)
    #js {:tag "init-swipl-with-prelude-url"
         :payload (m/some ?swipl-prelude-qlf-url)}
    (prom/let
     [swipl' (swipl-wasm-query/init-swipl! ?swipl-prelude-qlf-url)]
      (reset! swipl swipl')
      (post-ready!))

    #js {:tag "run-l4-query"
         :payload (m/some ?l4-program)}
    (->> ?l4-program
         transpile-and-query!
         (prom/hcat #(post-ready!)))

    _ (post-ready!)))

(defn init-worker! []
  ;; Ugly hack to get swipl wasm working in a web worker without access
  ;; to js/window.
  ;; The issue is that otherwise, it fails to load prolog and qlf file via
  ;; Prolog.consult with following error:
  ;; ERROR: JavaScript: ReferenceError: window is not defined
  ;; To solve this, we assign a global window object to an empty object just so
  ;; that it's defined.
  (jsi/assoc! js/globalThis :window #js {})
  ;; For some reason, (set! js/onmessage ...) yields the following error when
  ;; compiled with :optimizations :advanced
  ;;   constant onmessage assigned a value more than once.
  ;;   Original definition at externs.shadow.js:7
  ;; To workaround this, we add an event handler via addEventListener instead. 
  (jsi/call js/globalThis :addEventListener "message" on-message!))