(ns l4-lp.ide.browser-backend.llm.core
  (:require [l4-lp.llm.js.wasm-core :refer [init-llm-engine!]]))

(def llm-engine
  (init-llm-engine!))