(ns l4-lp.ide.ui.utils 
  (:require ["@mui/material/CircularProgress$default" :as CircularProgress]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]
            [uix.core :as uix]))

(uix/defui suspense-loading-bar
  [{:keys [children]}]
  (uix/$ uix/suspense {:fallback (uix/$ CircularProgress)} children))

(defn- fetch-as-text!
  [url]
  (it-> url
        (fetch/get it {:content-type :text})
        (prom/map :body it)))

(defn use-fetch-as-text!
  [url]
  (let [[text set-text!] (uix/use-state (uix/$ CircularProgress))
        [fetched? set-fetched!] (uix/use-state false)]

    (uix/use-effect
     #(prom/let [text (fetch-as-text! url)]
        (set-text! text)
        (set-fetched! true))
     [url])

    [text fetched?]))

(defn use-web-worker!
  [js-script-url
   & {:keys [worker-opts]}]
  (let [worker-ref (uix/use-ref)
        [worker-ready? set-worker-ready!] (uix/use-state false)]

    (uix/use-effect
     (fn []
       (let [opts (-> worker-opts bean/->js (jsi/assoc! :type "module"))
             worker (new js/Worker js-script-url opts)]
         (reset! worker-ref worker)
         (set-worker-ready! true))
       #(jsi/call @worker-ref :terminate))

     [js-script-url worker-opts])

    [worker-ref worker-ready?]))