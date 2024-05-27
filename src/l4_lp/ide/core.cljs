(ns l4-lp.ide.core
  (:require [l4-lp.ide.ui.core :refer [render-ide-app!]]))

(def ^:private ide-app-id
  "ide-app")

(defn start! []
  (println "Starting..."))

(defn stop! []
  (println "Stopping..."))

(defn init! []
  (render-ide-app! ide-app-id)
  (start!))