(ns l4-lp.ide.query-worker 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]))

(defn- post-data-as-js! [& {:keys [tag payload]}]
  (js/postMessage #js {:tag (bean/->js tag)
                       :payload (bean/->js payload)}))

(defn transpile-and-query! [^js event]
  (let [l4-program (jsi/get event :data)
        transpiled-prolog (-> l4-program
                              l4->prolog/l4->prolog-program+queries)]
    (prom/do
      (post-data-as-js! :tag "transpiled-prolog" :payload transpiled-prolog)

      ;; Ugly hack to get swipl wasm working in a web worker without access
      ;; to js/window.
      ;; The issue is that it fails to load prolog and qlf files on nodejs via Prolog.consult
      ;; with following error:
      ;; ERROR: JavaScript: ReferenceError: window is not defined
      ;; To solve this, we assign a global window object to an empty object just so
      ;; that it's defined.
      (jsi/assoc! js/globalThis :window #js {})

      (swipl-wasm-query/query-and-trace!
       transpiled-prolog
       #(post-data-as-js! :tag "query-result" :payload %))

      (post-data-as-js! :tag "done" :payload nil))))

(defn init! []
  (set! js/onmessage transpile-and-query!))