(ns l4-lp.ide.browser-backend.worker.core
  (:require [applied-science.js-interop :as jsi]
            [l4-lp.ide.browser-backend.worker.utils :as worker-utils]
            [l4-lp.swipl.js.wasm-query :as swipl-wasm-query]
            [l4-lp.syntax.l4-to-prolog :as l4->prolog]
            [meander.epsilon :as m]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(def ^:private swipl
  (atom nil))

(defn- transpile-and-query! [l4-program]
  (let [transpiled-prolog (-> l4-program
                              l4->prolog/l4->prolog-program+queries)]
    (worker-utils/post-data-as-js!
     :tag "transpiled-prolog" :payload transpiled-prolog)

    (it-> transpiled-prolog
          (swipl-wasm-query/query-and-trace!
           it
           :swipl @swipl
           :on-query-result
           #(worker-utils/post-data-as-js! :tag "query-result" :payload %))
          (prom/hcat #(worker-utils/post-done!) it))))

(defn ^:private on-message! [event]
  (m/match (jsi/get event :data)
    #js {:tag "init-swipl-with-prelude-url"
         :payload (m/some ?swipl-prelude-qlf-url)}
    (do (->> ?swipl-prelude-qlf-url
             swipl-wasm-query/init-swipl!
             (reset! swipl))
        (worker-utils/post-done!))

    #js {:tag "run-l4-query"
         :payload (m/some ?l4-program)}
    (transpile-and-query! ?l4-program)

    _ (worker-utils/post-done!)))

(defn init! []
  ;; Ugly hack to get swipl wasm working in a web worker without access
  ;; to js/window.
  ;; The issue is that otherwise, it fails to load prolog and qlf file via
  ;; Prolog.consult with following error:
  ;; ERROR: JavaScript: ReferenceError: window is not defined
  ;; To solve this, we assign a global window object to an empty object just so
  ;; that it's defined.
  (jsi/assoc! js/globalThis :window #js {})
  (worker-utils/init-worker! on-message!))