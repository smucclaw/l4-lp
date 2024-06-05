(ns l4-lp.ide.browser-backend.worker.utils 
  (:require [applied-science.js-interop :as jsi]
            [cljs-bean.core :as bean]))

(defn post-data-as-js! [& {:keys [tag payload]}]
  (let [js-data (bean/->js {:tag tag :payload payload})]
    (js/postMessage js-data)))

(defn post-done! []
  (js/postMessage nil))

(defn init-worker! [on-message]
   ;; For some reason, (set! js/onmessage ...) yields the following error when
  ;; compiled with :optimizations :advanced
  ;;   constant onmessage assigned a value more than once.
  ;;   Original definition at externs.shadow.js:7
  ;; To workaround this, we add an event handler via addEventListener instead. 
  (jsi/call js/globalThis :addEventListene "message" on-message))