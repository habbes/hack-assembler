(ns hack-assembler.parser
  (:require [clojure.string :as s]
            [clojure.context :as ctx]))

; regex used to parce an A instruction
(def a-inst-re #"@([\w\:\.\$]+)")
; regex used to parse a C instruction
(def c-inst-re #"(?:([AMD]{1,3})\=)?([AMD01+\-]+)(?:;([A-Z]{3}))?")
; regex used to parse an L pseudocommand
(def l-inst-re #"\(([\w\:\.\$]+)\)")

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

(defn parse-l-instruction-first-pass
  "Parses the source assembly if it's label pseudocommand (LOOP).
  Returns the updated context"
  [source {:keys [line-number instruction-number symbol-table] :as context}]
  (if-let [[_ label] (re-matches l-inst-re source)]
      (-> context
          (ctx/update-table conj [label (inc instruction-number)]))        
      context))

(defn parse-instruction-first-pass
  "Parses the assembly source for the first pass, to save address of labels"
  [source context]
  (if (= source "")
    context
    (if (= (subs source 0 1) "(")
        (parse-l-instruction-firstpass source context)
        context))

(defn parse-line-first-pass
  "Parses line of assembly source during the first pass, considering only label
  pseudo-commands"
  [source context]
  (let [extracted (extract-instruction source)]
    (parse-instruction-first-pass extracted context)))