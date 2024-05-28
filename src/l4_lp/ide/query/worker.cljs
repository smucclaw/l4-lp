(ns l4-lp.ide.query.worker 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(defn- post-data-as-js! [& {:keys [tag payload]}]
  (js/postMessage #js {:tag (bean/->js tag)
                       :payload (bean/->js payload)}))

(defn- transpile-and-query! [^js event]
  (jsi/let [^:js {{:keys [l4-program swipl-prelude-qlf-url]} :data} event
            transpiled-prolog (-> l4-program
                                  l4->prolog/l4->prolog-program+queries)]
    (post-data-as-js! :tag "transpiled-prolog" :payload transpiled-prolog)

    (it-> transpiled-prolog
        (swipl-wasm-query/query-and-trace!
         it
         :swipl-prelude-qlf-url swipl-prelude-qlf-url
         :query-result-callback
         #(post-data-as-js! :tag "query-result" :payload %))
        (prom/hcat #(post-data-as-js! :tag "done") it))))

(defn init! []
  ;; Ugly hack to get swipl wasm working in a web worker without access
  ;; to js/window.
  ;; The issue is that otherwise, it fails to load prolog and qlf file via
  ;; Prolog.consult with following error:
  ;; ERROR: JavaScript: ReferenceError: window is not defined
  ;; To solve this, we assign a global window object to an empty object just so
  ;; that it's defined.
  (jsi/assoc! js/globalThis :window #js {})
  (set! js/onmessage transpile-and-query!))