(ns l4-lp.syntax.arithmetic 
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "expr = add-sub
    <add-sub> = mul-div | add | sub
    add = add-sub <'+'> mul-div
    sub = add-sub <'-'> mul-div
    <mul-div> = term | mul | div
    mul = mul-div <'*'> term
    div = mul-div <'/'> term
    <term> = number | <'('> add-sub <')'>
    number = #'[0-9]+'"))

(defn parse-str [s]
  (insta/parses parser s))