(ns l4-lp.ide.browser-backend.llama.core
  (:require [l4-lp.llama.js.wasm-core :refer [init-wllama!]]))

(def wllama
  (init-wllama!))