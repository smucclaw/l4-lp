(ns l4-lp.ide.query.worker 
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.ide.query.utils :refer [post-data-as-js!]]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [meander.epsilon :as m]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(def ^:private swipl-prelude-qlf-url
  (atom nil))

(defn- transpile-and-query! [l4-program]
  (let [transpiled-prolog (-> l4-program
                              l4->prolog/l4->prolog-program+queries)]
    (post-data-as-js! :tag "transpiled-prolog" :payload transpiled-prolog)

    (it-> transpiled-prolog
          (swipl-wasm-query/query-and-trace!
           it
           :swipl-prelude-qlf-url @swipl-prelude-qlf-url
           :on-query-result
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

  (set! js/onmessage
        (jsi/fn [^:js {:keys [data]}]
          (m/match data
            #js {:tag (m/some "swipl-prelude-qlf-url")
                 :payload (m/some ?url)}
            (reset! swipl-prelude-qlf-url ?url)

            #js {:tag (m/some "l4-program")
                 :payload (m/some ?l4-program)}
            (transpile-and-query! ?l4-program)))))