(ns l4-lp.ide.ui.query.output.guifier 
  (:require ["https://cdn.jsdelivr.net/npm/guifier@1.0.27/dist/Guifier.js$default"
             :as Guifier]
            ["@mui/material/Box$default" :as Box]
            [applied-science.js-interop :as jsi]
            [uix.core :as uix]))

(defn- init-guifier!
  ([elt-id]
   (init-guifier! elt-id nil))

  ([elt-id js-data]
   (let [escaped-elt-id (->> elt-id
                             (jsi/call js/CSS :escape)
                             (str "#"))]
     (new Guifier
          #js {:data js-data
               :dataType "js"
               :elementSelector escaped-elt-id
               :withoutContainer true
               :readOnlyMode true}))))

(uix/defui guifier
  [{:keys [data box-props]}]
  (let [elt-id (uix/use-id)
        guifier-callback-ref
        (uix/use-callback #(init-guifier! elt-id data)
                          [elt-id data])
        box-props (merge box-props
                         {:id elt-id :ref guifier-callback-ref})]
    (uix/$ Box box-props)))