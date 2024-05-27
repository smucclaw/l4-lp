(ns l4-lp.ide.ui.utils 
  (:require ["@mui/material/CircularProgress$default" :as CircularProgress]
            [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]
            [uix.core :as uix]))

(defn fetch-as-text!
  [url]
  (it-> url
        (fetch/get it {:content-type :text})
        (prom/map :body it)))

(defn memoised-fetch-as-text! [url]
  (uix/use-memo #(fetch-as-text! url) [url]))

(uix/defui suspense-loading-bar
  [{:keys [children]}]
  (uix/$ uix/suspense {:fallback (uix/$ CircularProgress)} children))