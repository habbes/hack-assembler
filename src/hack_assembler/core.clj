(ns hack-assembler.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hack-assembler.parser :as parser]
            [hack-assembler.code :as code])
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

(defn -main
  "Reads the source assembly file passed in the arguments and outputs
  a file containing the Hack machine code
  Xxx.asm -> Xxx.hack"
  [source-file & rest]
  (let [filename (-> source-file
                  (str/split #"\.")
                  first)
        output-file (str filename ".hack")]
       (with-open [rdr (io/reader source-file)]
         (with-open [wrtr (io/writer output-file)]
           (println "Compiling to " output-file)
           (translate-source rdr wrtr)
           (println "Operation complete."))))) 
       
