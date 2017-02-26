(ns hack-assembler.parser
  (:require [clojure.string :as s]))

(def a-inst-re #"@(\w+)")
; regex used to parse a c instruction
(def c-inst-re #"(?:([AMD]{1,3})\=)?([AMD01+\-]+)(?:;([A-Z]{3}))?")

(declare parse-a-instruction)
(declare parse-c-instruction)

(defn extract-instruction
  "extract instruction from the source line, ignoring comments and whitespace.
  If no instruction is in the supplied line an empty string is returned."
  [source]
  (-> source
      (s/split #"//")                                       ; split instruction and comment
      first                                                 ; ignore comment
      s/trim                                                ; remove outer whitespace
      (s/replace #"\s+" "")))                                 ; remove internal whitespace
      
(defn parse-instruction
  "parses the source assembly string into a command map
  containing the type and parts of the instruction"
  [source]
  (if (= source "")
    nil
    (if (= (subs source 0 1) "@")
      (parse-a-instruction source)
      (parse-c-instruction source))))

(defn parse-a-instruction
  "parses the source assembly into an A Instruction map
  containing the type A_COMMAND and the address part"
  [source]
  (if-let [[_ label] (re-matches a-inst-re source)]
    {:type    "A_COMMAND"
     :address (Integer/parseInt label)} 
    nil))

(defn parse-c-instruction
  "parses the source assembly string into a C Instruction map
  containing the type C_COMMAND and the dest, comp and jump parts"
  [source]
  (if-let [[_ dest comp jump] (re-matches c-inst-re source)]
    {:type "C_COMMAND"
     :dest dest
     :comp comp
     :jump jump} 
    nil))
    
(defn parse-line
  "Parses an instruction from source line. If the line contains
  a command, a command map is returned. If the line contains
  only comments or whitespace, nil is returned"
  [source]
  (-> source
      extract-instruction
      parse-instruction))