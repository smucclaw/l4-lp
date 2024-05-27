(ns l4-lp.ide.ui.top-bar 
  (:require ["@mui/icons-material/GitHub$default" :as GitHubIcon]
            ["@mui/material/AppBar$default" :as AppBar]
            ["@mui/material/Box$default" :as Box]
            ["@mui/material/Link$default" :as Link]
            ["@mui/material/Toolbar$default" :as Toolbar]
            ["@mui/material/Typography$default" :as Typography]
            [uix.core :as uix]))

(uix/defui top-bar
  [{:keys [children]}]
  (uix/$ Box {:flex-grow 1}
         (uix/$
          AppBar {:position :fixed}
          (uix/$ Toolbar {:variant :dense}
                 (uix/$ Link {:color :inherit
                              :href "https://github.com/smucclaw/l4-lp"}
                        (uix/$ GitHubIcon))
                 (uix/$ Typography {:mt 1 :ml 5 :mr 5
                                    :variant :h5 :gutter-bottom true}
                        "L4 IDE")
                children))
         (uix/$ Toolbar)))