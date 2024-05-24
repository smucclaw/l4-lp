(ns l4-lp.web-editor.utils 
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

(uix/defui loading-bar
  [{:keys [children]}]
  (uix/$ uix/suspense {:fallback (uix/$ CircularProgress)} children))