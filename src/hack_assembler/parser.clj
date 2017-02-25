(ns hack-assembler.parser
  (:require [clojure.string :as s]))

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
      (s/replace #"\s+" "")                                 ; remove internal whitespace
      ))
  
(defn parse-instruction
  "parses the source assembly string into a command map
  containing the type and parts of the instruction"
  [source]
  (if (= (subs source 0 1) "@")
    (parse-a-instruction source)
    (parse-c-instruction source)))

(defn parse-a-instruction
  "parses the source assembly into an A Instruction map
  containing the type A_COMMAND and the address part"
  [source]
  (let [address (subs source 1)]
    {:type    "A_COMMAND"
     :address address}))


(defn parse-c-instruction
  "parses the source assembly string into a C Instruction map
  containing the type C_COMMAND and the dest, comp and jump parts"
  [source]
  (let [[_ dest comp jump] (re-matches c-inst-re source)]
    {:type "C_COMMAND"
     :dest dest
     :comp comp
     :jump jump}))