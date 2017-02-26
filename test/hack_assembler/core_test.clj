(ns hack-assembler.core-test
  (:require [clojure.test :refer :all]
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
