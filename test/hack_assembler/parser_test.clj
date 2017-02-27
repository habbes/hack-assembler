(ns hack-assembler.parser-test
  (:require [clojure.test :refer :all]
            [hack-assembler.parser :refer :all]))

; sample context used throughout
(def sample-ctx {:instruction-number 10 
                 :line-number 12 
                 :symbol-table {:cur-var-address 16 "LOOP" 23}})

(deftest extract-instruction-test
  (testing "instruction only"
    (let [instruction (extract-instruction "D=1;JMP")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "@1")]
         (is (= instruction "@1"))))
    
  (testing "ignores comments"
    (let [instruction (extract-instruction "D=1;JMP// this is a comment")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "@20// this is a comment")]
         (is (= instruction "@20"))))
  (testing "ignores whitespace"
    (let [instruction (extract-instruction "  D=1;JMP  ")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "  D=1;  JMP")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "D = 1;JMP")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "  @2405 ")]
         (is (= instruction "@2405"))))
  (testing "ignores comments and whitespace"
    (let [instruction (extract-instruction "D = 1;JMP // this is a comment")]
      (is (= instruction "D=1;JMP"))))
  (testing "returns empty string when no instruction is supplied"
    (let [instruction (extract-instruction " ")]
      (is (= instruction "")))
    (let [instruction (extract-instruction "        ")]
      (is (= instruction "")))
    (let [instruction (extract-instruction "// test")]
      (is (= instruction "")))
    (let [instruction (extract-instruction "  // test")]
      (is (= instruction "")))))

(deftest parse-a-instruction-test
  (testing "command type is A_COMMAND"
    (is (= (:type (nth (parse-a-instruction "@256" sample-ctx) 0)) "A_COMMAND")))
  (testing "Resolves constant address"
    (let [[{:keys [address]} ctx] (parse-a-instruction "@1234" sample-ctx)]
      (is (= address 1234))
      (is (= ctx sample-ctx)))))

(deftest parse-c-instruction-test
  (testing "command type is C_COMMAND"
    (let [[{:keys [type]} ctx] (parse-c-instruction "D=A+1;JMP" sample-ctx)]
      (is (= type "C_COMMAND"))))
  (testing "separates dest, comp and jump parts"
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "A=D+1;JMP" sample-ctx)]
      (is (= [dest comp jump ctx] ["A" "D+1" "JMP" sample-ctx])))
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "AMD=0;JEQ" sample-ctx)]
      (is (= [dest comp jump] ["AMD" "0" "JEQ"])))
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "M=D-M;JNE" sample-ctx)]
      (is (= [dest comp jump] ["M" "D-M" "JNE"]))))
  (testing "sets dest to nil when not supplied"
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "1;JGE" sample-ctx)]
      (is (= [dest comp jump] [nil "1" "JGE"])))
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "D+1;JEQ" sample-ctx)]
      (is (= [dest comp jump] [nil "D+1" "JEQ"]))))
  (testing "sets jump to nil when not supplied"
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "D=1" sample-ctx)]
      (is (= [dest comp jump] ["D" "1" nil])))
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "M=D+1" sample-ctx)]
      (is (= [dest comp jump] ["M" "D+1" nil]))))
  (testing "sets dest and jump to nil when only comp supplied"
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "1" sample-ctx)]
      (is (= [dest comp jump] [nil "1" nil])))
    (let [[{:keys [dest comp jump]} ctx] (parse-c-instruction "D+1" sample-ctx)]
      (is (= [dest comp jump] [nil "D+1" nil])))))

(deftest parse-instruction-test
  (testing "parse A instruction"
    (let [[{:keys [type address]} ctx] (parse-instruction "@123" sample-ctx)]
      (is (= type "A_COMMAND"))
      (is (= address 123))))
  (testing "parse C instruction")
  (let [[{:keys [type dest comp jump]} ctx] (parse-instruction "D=1" sample-ctx)]
    (is (= type "C_COMMAND"))
    (is (= [dest comp jump] ["D" "1" nil])))
  (testing "returns nil if instruction is empty"
    (let [instruction (parse-instruction "" sample-ctx)]
      (is (= instruction nil)))))

(deftest parse-line-test
  (testing "parse line with instruction"
    (let [[{:keys [type address]} ctx] (parse-line " @123 // store address 123" sample-ctx)]
      (is (= type "A_COMMAND"))
      (is (= address 123)))
    (let [[{:keys [type dest comp jump]} ctx] (parse-line " D=1 // store 1 in D" sample-ctx)]
      (is (= type "C_COMMAND"))
      (is (= [dest comp jump] ["D" "1" nil]))))
  (testing "parse line without instruction"
    (let [instruction (parse-line "// this is a test" sample-ctx)]
      (is (= instruction nil)))
    (let [instruction (parse-line "   " sample-ctx)]
      (is (= instruction nil)))))