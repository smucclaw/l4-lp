(ns l4-lp.ide.ui.query.output.guifier 
  (:require ["https://cdn.jsdelivr.net/npm/guifier@1.0.24/dist/Guifier.js$default"
             :as Guifier]
            ["@mui/material/Box$default" :as Box]
            [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]
            [uix.core :as uix]))

(defn- init-guifier!
  ([guifier-elt-id]
   (init-guifier! guifier-elt-id nil))
  
  ([guifier-elt-id data]
   (new Guifier
        #js {:data (bean/->js data)
             :dataType "js"
             :elementSelector (->> guifier-elt-id
                                   (jsi/call js/CSS :escape)
                                   (str "#"))
             :withoutContainer true
             :readOnlyMode true})))

(uix/defui guifier
  [{:keys [data box-props]}]

  (let [elt-id (str "guifier" (uix/use-id))
        guifier-callback-ref
        (uix/use-callback #(init-guifier! elt-id data)
                          [elt-id data])]
    (uix/$ Box
           (merge box-props
                  {:ref guifier-callback-ref :id elt-id}))))