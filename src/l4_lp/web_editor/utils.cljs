(ns l4-lp.web-editor.utils 
  (:require [lambdaisland.fetch :as fetch]
            [promesa.core :as prom]
            [tupelo.core :refer [it->]]))

(defn fetch-text-from-url-and-then! [url f]
 (it-> url
       (fetch/get it {:content-type :text})
       (prom/map :body it)
       (prom/map f it)))