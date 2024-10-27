(ns l4-lp.ide.ui.query.button 
  (:require ["@mui/lab/LoadingButton$default" :as LoadingButton]
            [uix.core :as uix]))

(uix/defui query-button
  [{:keys [on-click loading?
           button-props children]}]
  (uix/$ LoadingButton
         (merge button-props
                {:color :secondary
                 :loading loading?
                 :on-click on-click})
         children))