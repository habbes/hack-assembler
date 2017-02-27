(ns hack-assembler.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hack-assembler.parser :as parser]
            [hack-assembler.code :as code]
            [hack-assembler.context :as context]
            [hack-assembler.symbol-table :as symt])
  (:gen-class))

(defn translate-line
  "Translates a line of source assembly code into
  the corresponding machine binary string"
  [line ctx]
  (if-let [[cmd parsed-ctx] (parser/parse-line line ctx)]
    [(code/translate-instruction cmd) parsed-ctx]
   [nil ctx]))

(defn translate-source
  "Reads source code from the rdr and writes the resulting
  binary code to wrtr"
  [rdr wrtr ctx]
  (let [lines (line-seq rdr)]
    (loop [count 0 ctx ctx]
      (if-let [line (nth lines count nil)]
        (if-let [[instruction updated-ctx] (translate-line line ctx)]
          (do
            (if instruction (.write wrtr (str instruction "\n")))
            (recur (inc count) updated-ctx)))
        nil))))

(defn preprocess-source
  "Reads source code from rdr and preprocess
  it by populating symbol table with label address. Returns
  the updated context."
  [rdr ctx]
  (let [lines (line-seq rdr)]
    (loop [count 0 ctx ctx]
      (if-let [line (nth lines count nil)]
        (recur
          (inc count)
          (parser/parse-line-first-pass line ctx))
       nil))))
       
(defn -main
  "Reads the source assembly file passed in the arguments and outputs
  a file containing the Hack machine code
  Xxx.asm -> Xxx.hack"
  [source-file & rest]
  (let [filename (-> source-file
                  (str/split #"\.")
                  first)
        output-file (str filename ".hack")
        table (symt/initialize-table)
        ctx (context/initialize-context table)
        ctx (with-open [rdr (io/reader source-file)] (preprocess-source rdr ctx))]
       (with-open [rdr (io/reader source-file)]
         (with-open [wrtr (io/writer output-file)]
           (println "Compiling to " output-file)
           (translate-source rdr wrtr ctx)
           (println "Operation complete."))))) 
