(ns l4-lp.llm.js.wasm-core 
  (:require ["@mlc-ai/web-llm" :as webllm]))

(def ^:private model
  "Llama-3-8B-Instruct-q4f32_1-MLC")

(defn init-llm-engine!
  ([] (init-llm-engine! identity))
  ([init-progress-callback]
   (webllm/CreateMLCEngine model
                           #js {:initProgressCallback init-progress-callback})))