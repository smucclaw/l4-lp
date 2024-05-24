(ns l4-lp.web-editor.utils 
  (:require [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(defn fetch-text!
  [url]
  (it-> url
        (fetch/get it {:content-type :text})
        (prom/map :body it)))