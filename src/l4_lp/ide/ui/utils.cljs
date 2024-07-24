(ns l4-lp.ide.ui.utils 
  (:require ["@mui/material/CircularProgress$default" :as CircularProgress]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [promesa.exec.csp :as csp]
            [tupelo.core :refer [it->]]
            [uix.core :as uix]
            [uix.dom :as uix-dom]))

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

    {:text text :fetched? fetched?}))

(defn- add-to-chan! [set-chan! data]
  (set-chan! (fn [chan]
               (prom/chain chan
                           #(csp/put % data)
                           (constantly chan)))))

(defn use-web-worker!
  [js-script-url
   & {:keys [worker-opts in-chan-size out-chan-size]
      :or {in-chan-size 10
           out-chan-size 10}}]
  (let [worker-ref (uix/use-ref)
        [worker-state set-worker-state!] (uix/use-state nil)
        [input-chan set-input-chan!] (uix/use-state (csp/chan :buf in-chan-size))
        [output-chan set-output-chan!] (uix/use-state (csp/chan :buf out-chan-size))]

    (uix/use-effect
     #(fn []
        (jsi/call @worker-ref :terminate)
        (set-worker-state! :stopped))
     [])

    (uix/use-effect
     #(let [opts (-> worker-opts bean/->js (jsi/assoc! :type "module"))
            worker (new js/Worker js-script-url opts)]
        (jsi/assoc! worker :onmessage
                    (jsi/fn [^:js {:keys [data]}]
                      (if data
                        (add-to-chan! set-output-chan! data)
                        (set-worker-state! :ready))))
        (reset! worker-ref worker)
        (set-worker-state! :ready))
     [js-script-url worker-opts])

     (uix/use-effect
      #(when (= worker-state :ready)
         (prom/let [worker @worker-ref
                    input-chan input-chan
                    js-data (csp/take input-chan)]
           (set-worker-state! :busy)
           (jsi/call worker :postMessage js-data)))
      [worker-state input-chan output-chan])

    {:worker-state worker-state
     :post-to-worker #(add-to-chan! set-input-chan! %)
     :output-chan output-chan}))

(defn render-app! [app-id elt]
  (let [app-root
        (-> js/document
            (jsi/call :getElementById app-id)
            uix-dom/create-root)]
    (uix-dom/render-root elt app-root)))