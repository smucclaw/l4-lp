(ns l4-lp.llama.js.wasm-core 
  (:require ["@wllama/wllama" :refer [Wllama]]
            ["@wllama/wllama/src/multi-thread/wllama.js?url$default"
             :as wllamaMultiJS]
            ["@wllama/wllama/src/multi-thread/wllama.wasm?url$default"
             :as wllamaMulti]
            ["@wllama/wllama/src/multi-thread/wllama.worker.mjs?url$default"
             :as wllamaMultiWorker]
            ["@wllama/wllama/src/single-thread/wllama.js?url$default"
             :as wllamaSingleJS]
            ["@wllama/wllama/src/single-thread/wllama.wasm?url$default"
             :as wllamaSingle]
            [applied-science.js-interop :as jsi]
            [promesa.core :as prom]))

(def ^:private config-paths
  #js {"single-thread/wllama.js " wllamaSingleJS
       "single-thread/wllama.wasm" wllamaSingle
       "multi-thread/wllama.js" wllamaMultiJS
       "multi-thread/wllama.wasm" wllamaMulti
       "multi-thread/wllama.worker.mjs" wllamaMultiWorker})

(def ^:private completion-model
  "https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories15M-q4_0.gguf")

(defn init-wllama! []
  (prom/let [wllama (new Wllama config-paths)
             _ (jsi/call wllama :loadModelFromUrl completion-model)]
    wllama))