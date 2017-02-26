(ns hack-assembler.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [hack-assembler.core :refer :all]))

(deftest translate-line-test
  (testing "Translates line of source code to binary string"
    (let [code (translate-line "@16 //test")]
      (is (= "0000000000010000" code)))
    (let [code (translate-line "  D=1 //test")]
         (is (= "1110111111010000" code))))
  (testing "Returns nil if no instruction in source"
    (let [code (translate-line "//test")]
         (is (= nil code)))
    (let [code (translate-line "  ")]
         (is (= nil code))))
  (testing "Returns nil on syntax error"
    (let [code (translate-line "A= //test")]
         (is (= nil code)))
    (let [code (translate-line "  D=1;")]
         (is (= nil code)))))

(deftest translate-source-test
  (testing "Translates source code from reader and write machine code output to writer"
    (let [in (io/reader (java.io.StringReader. "@2\nD=A;JMP"))
          out (java.io.StringWriter.)]
         (translate-source in out)
         (let [output (.toString out)]
           (is (= "0000000000000010\n1110110000010111\n" output)))))
  (testing "Translates source code containing comments and whitespace"
    (let [in (io/reader (java.io.StringReader. "//this is a test\n\n@2//set address to 2\nD=A;JMP"))
          out (java.io.StringWriter.)]
         (translate-source in out)
         (let [output (.toString out)]
           (is (= "0000000000000010\n1110110000010111\n" output))))))
         
    
