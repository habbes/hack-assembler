(ns hack-assembler.parser)

; regex used to parse a c instruction
(def c-inst-re #"(?:([AMD]{1,3})\=)?([AMD01+\-]+)(?:;([A-Z]{3}))?")

(defn parse-a-instruction
  [source]
  (let [address (subs source 1)]
    {:type    "A_COMMAND"
     :address address}))


(defn parse-c-instruction
  [source]
  (let [[_ dest comp jump] (re-matches c-inst-re source)]
    {:type "C_COMMAND"
     :dest dest
     :comp comp
     :jump jump}))