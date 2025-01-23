(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/material/Button$default" :as Button]
            [uix.core :as uix]))

(uix/defui query-button
  [{:keys [on-click loading?
           button-props children]}]
  (uix/$ Button
         (merge button-props
                {:color :secondary
                 :loading loading?
                 :on-click on-click})
         children))