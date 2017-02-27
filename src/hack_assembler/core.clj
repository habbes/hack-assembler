(ns hack-assembler.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hack-assembler.parser :as parser]
            [hack-assembler.code :as code]
            [hack-assembler.context :as context])
  (:gen-class))

(defn translate-line
  "Translates a line of source assembly code into
  the corresponding machine binary string"
  [line]
  (-> line
    parser/parse-line
    code/translate-instruction))

(defn translate-source
  "Reads source code from the rdr and writes the resulting
  binary code to wrtr"
  [rdr wrtr]
  (doseq [line (line-seq rdr)]
    (if-let [instruction (translate-line line)]
     (.write wrtr (str instruction "\n")))))

(defn preprocess
  "Reads source code from rdr and preprocess
  it by populating symbol table with label address. Returns
  the updated context."
  [rdr ctx]
  (let [line (line-seq rdr)]
    (if-let [[_ ctx] (parser/parse-line-first-pass line ctx)]
      (-> ctx
          context/inc-line
          context/inc-instruction))))

(defn first-pass
  ""
  [rdr ctx]
  (let [lines (line-seq rdr)]
    (loop [count 1 ctx ctx]
      (if-let [line (nth lines count nil)]
        (recur
          (inc count)
          (parser/parse-line-first-pass line ctx))
      nil)))
       

(defn -main
  "Reads the source assembly file passed in the arguments and outputs
  a file containing the Hack machine code
  Xxx.asm -> Xxx.hack"
  [source-file & rest]
  (let [filename (-> source-file
                  (str/split #"\.")
                  first)
        output-file (str filename ".hack")
        ctx (context/initialize-context)
        ctx (with-open [rdr (io/reader source-file)] (first-pass rdr ctx))]
       (with-open [rdr (io/reader source-file)]
         (with-open [wrtr (io/writer output-file)]
           (println "Compiling to " output-file)
           (translate-source rdr wrtr)
           (println "Operation complete."))))) 
       
