(ns hack-assembler.core
  (:require [hack-assembler.parser :as parser]
    [hack-assembler.code :as code])
  (:gen-class))

(defn translate-line
  "Translates a line of source assembly code into
  the corresponding machine binary string"
  [line]
  (-> line
    parser/parse-line
    code/translate-instruction))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
