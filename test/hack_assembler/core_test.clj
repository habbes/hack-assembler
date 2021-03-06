(ns hack-assembler.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [hack-assembler.core :refer :all]))

; sample context used in most tests
(def sample-ctx {:line-number 10 :instruction-number 5 :symbol-table {:cur-var-address 16}})

(deftest translate-line-test
  (testing "Translates line of source code to binary string"
    (let [[code ctx] (translate-line "@16 //test" sample-ctx)]
      (is (= "0000000000010000" code))
      (is (= {:line-number 10
              :instruction-number 5
              :symbol-table {:cur-var-address 16}} ctx)))
    (let [[code ctx] (translate-line "@i //test" sample-ctx)]
      (is (= "0000000000010000" code))
      (is (= {:line-number 10
              :instruction-number 5
              :symbol-table {:cur-var-address 17 "i" 16}} ctx)))
    (let [[code ctx] (translate-line "  D=1 //test" sample-ctx)]
      (is (= "1110111111010000" code))
      (is (= {:line-number 10
              :instruction-number 5
              :symbol-table {:cur-var-address 16}} ctx))))
  (testing "Returns nil if no instruction in source"
    (let [[code ctx] (translate-line "//test" sample-ctx)]
       (is (= nil code))
       (is (= ctx sample-ctx)))
    (let [[code ctx] (translate-line "  " sample-ctx)]
       (is (= nil code))
       (is (= ctx sample-ctx))))
  (testing "Returns nil on syntax error"
    (let [[code ctx] (translate-line "A= //test" sample-ctx)]
       (is (= nil code))
       (is (= ctx sample-ctx)))
    (let [[code ctx] (translate-line "  D=1;" sample-ctx)]
       (is (= nil code))
       (is (= ctx sample-ctx)))))

(deftest translate-source-test
  (testing "Translates source code from reader and write machine code output to writer"
    (let [in (io/reader (java.io.StringReader. "@2\nD=A;JMP\n@i\nM=A\n@END\n0;JMP\n(END)"))
          out (java.io.StringWriter.)
          ctx {:instruction-number 0 :line-number 1 :symbol-table {:cur-var-address 16 "END" 6}}] 
         (translate-source in out ctx)
         (let [output (.toString out)]
           (is (= (str "0000000000000010\n"
                       "1110110000010111\n"
                       "0000000000010000\n"
                       "1110110000001000\n"
                       "0000000000000110\n"
                       "1110101010000111\n")
                output)))))
  (testing "Translates source code containing comments and whitespace"
    (let [in (io/reader (java.io.StringReader. "//this is a test\n\n@var1\n@var2\n@2//set address to 2\nD=A;JMP\n@var1"))
          out (java.io.StringWriter.)
          ctx {:instruction-number 0 :line-number 1 :symbol-table {:cur-var-address 16}}] 
         (translate-source in out ctx)
         (let [output (.toString out)]
           (is (= (str "0000000000010000\n"
                       "0000000000010001\n"
                       "0000000000000010\n"
                       "1110110000010111\n"
                       "0000000000010000\n")
                  output))))))
         
(deftest preprocess-source-test
  (testing "Build symbol table populated with label addresses from source code"
    (let [in (io/reader (java.io.StringReader. 
                         "//test\n@i\n@j\n(START)//start\nD=1\n@END\n0;JMP\n(END)"))
          ctx {:instruction-number 0 :line-number 1 :symbol-table {:cur-var-address 16}}
          processed-ctx (preprocess-source in ctx)]
         (is (= {:cur-var-address 16 "START" 2 "END" 6})))))
             