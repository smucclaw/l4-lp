(ns l4-lp.ide.query.utils 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]))

(def no-op
  (constantly nil))

(defn post-data-as-js! [& {:keys [worker tag payload]}]
  (let [post-message!
        (if worker
          #(jsi/call worker :postMessage %)
          js/postMessage)
        js-data (bean/->js {:tag tag :payload payload})]
    (post-message! js-data)))