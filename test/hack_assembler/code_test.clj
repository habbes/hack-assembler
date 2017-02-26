(ns hack-assembler.code-test
    (:require [clojure.test :refer :all]
        [hack-assembler.code :refer :all]))

(deftest translate-a-instruction-test
    (testing "translates A command to binary string"
        (let [cmd {:type "A_COMMAND" :address 200}]
            (is (= (translate-a-instruction cmd) "0000000011001000")))
        (let [cmd {:type "A_COMMAND" :address 1}]
            (is (= (translate-a-instruction cmd) "0000000000000001")))
        (let [cmd {:type "A_COMMAND" :address 32767}]
            (is (= "0111111111111111" (translate-a-instruction cmd))))))

(deftest translate-c-instruction-test
    (testing "translates C command to binary string"
        (let [cmd {:type "C_COMMAND" :dest "AMD" :comp "D+1" :jump "JGE"}]
            (is (= "1110011111111011" (translate-c-instruction cmd))))
        (let [cmd {:type "C_COMMAND" :dest nil :comp "M-D" :jump "JMP"}]
             (is (= "1111000111000111" (translate-c-instruction cmd))))
        (let [cmd {:type "C_COMMAND" :dest "M" :comp "1" :jump nil}]
             (is (= "1110111111001000" (translate-c-instruction cmd))))))

(deftest translate-instruction-test
    (testing "translates A command to binary string"
        (let [cmd {:type "A_COMMAND" :address 16}]
            (is (= "0000000000010000" (translate-instruction cmd)))))
    (testing "translates C command to binary string"
        (let [cmd {:type "C_COMMAND" :dest "D" :comp "A" :jump "JEQ"}]
            (is (= "1110110000010010" (translate-instruction cmd))))))
        
        
        