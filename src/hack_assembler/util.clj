(ns hack-assembler.util
    (:require [clojure.pprint :refer [cl-format]]))

(defn convert-to-binary
    "Convert n to a binary string representation of the
    specified width"
    ([n]
     (convert-to-binary n 15))
    ([n width]
     (let [format (str "~" width ",'0',B")]
         (cl-format nil format n)))) 
