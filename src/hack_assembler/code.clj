(ns hack-assembler.code
  (:require (hack-assembler.util :as util)))

(def dest-map
  {nil "000"
   "M" "001"
   "D" "010"
   "MD" "011"
   "A" "100"
   "AM" "101"
   "AD" "110"
   "AMD" "111"})
   
(def jump-map
  {nil "000"
   "JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLE" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(def comp-map
  {"0" "0101010"
   "1" "0111111"
   "-1" "0111010"
   "D" "0001100"
   "A" "0110000"
   "!D" "0001101"
   "!A" "0110001"
   "-D" "0001111"
   "-A" "0110011"
   "D+1" "0011111"
   "A+1" "0110111"
   "D-1" "0001110"
   "A-1" "0110010"
   "D+A" "0000010"
   "D-A" "0010011"
   "A-D" "0000111"
   "D&A" "0000000"
   "D|A" "0010101"
   "M" "1110000"
   "!M" "1110001"
   "-M" "1110011"
   "M+1" "1110111"
   "M-1" "1110010"
   "D+M" "1000010"
   "D-M" "1010011"
   "M-D" "1000111"
   "D&M" "1000000"
   "D|M" "1010101"})

(defn translate-c-instruction
  "Generate machine language binary representation
  of the given C command"
  [{:keys [dest comp jump]}]
  (let [dest-code (get dest-map dest)
        jump-code (get jump-map jump)
        comp-code (get comp-map comp]
        (str "111" comp-code dest-code jump-code)))

(defn translate-a-instruction
  "Generate machine language binaryrepresentation
  of the given A command"
  [{:keys [address]}]
  (let [bin-addr (util/convert-to-binary address)] 
    (str "0" bin-addr))

(defn translate-instruction
  [{type :type :as instruction}]
  (case type
    "A_COMMAND" (translate-a-instruction instruction)
    "C_COMMAND" (translate-c-instruction instruction)))
  
   
   
   