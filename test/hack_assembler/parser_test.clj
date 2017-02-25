(ns hack-assembler.parser-test
  (:require [clojure.test :refer :all]
            [hack-assembler.parser :refer :all]))

(deftest extract-instruction-test
  (testing "instruction only"
    (let [instruction (extract-instruction "D=1;JMP")]
      (is (= instruction "D=1;JMP"))))
  (testing "ignores comments"
    (let [instruction (extract-instruction "D=1;JMP// this is a comment")]
      (is (= instruction "D=1;JMP"))))
  (testing "ignores whitespace"
    (let [instruction (extract-instruction "  D=1;JMP  ")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "  D=1;  JMP")]
      (is (= instruction "D=1;JMP")))
    (let [instruction (extract-instruction "D = 1;JMP")]
      (is (= instruction "D=1;JMP"))))
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
    (is (= (:type (parse-a-instruction "@256")) "A_COMMAND")))
  (testing "gets address"
    (let [{:keys [address]} (parse-a-instruction "@1234")]
      (is (= address "1234")))))

(deftest parse-c-instruction-test
  (testing "command type is C_COMMAND"
    (let [{:keys [type]} (parse-c-instruction "D=A+1;JMP")]
      (is (= type "C_COMMAND"))))
  (testing "separates dest, comp and jump parts"
    (let [{:keys [dest comp jump]} (parse-c-instruction "A=D+1;JMP")]
      (is (= [dest comp jump] ["A" "D+1" "JMP"])))
    (let [{:keys [dest comp jump]} (parse-c-instruction "AMD=0;JEQ")]
      (is (= [dest comp jump] ["AMD" "0" "JEQ"])))
    (let [{:keys [dest comp jump]} (parse-c-instruction "M=D-M;JNE")]
      (is (= [dest comp jump] ["M" "D-M" "JNE"]))))
  (testing "sets dest to nil when not supplied"
    (let [{:keys [dest comp jump]} (parse-c-instruction "1;JGE")]
      (is (= [dest comp jump] [nil "1" "JGE"])))
    (let [{:keys [dest comp jump]} (parse-c-instruction "D+1;JEQ")]
      (is (= [dest comp jump] [nil "D+1" "JEQ"]))))
  (testing "sets jump to nil when not supplied"
    (let [{:keys [dest comp jump]} (parse-c-instruction "D=1")]
      (is (= [dest comp jump] ["D" "1" nil])))
    (let [{:keys [dest comp jump]} (parse-c-instruction "M=D+1")]
      (is (= [dest comp jump] ["M" "D+1" nil]))))
  (testing "sets dest and jump to nil when only comp supplied"
    (let [{:keys [dest comp jump]} (parse-c-instruction "1")]
      (is (= [dest comp jump] [nil "1" nil])))
    (let [{:keys [dest comp jump]} (parse-c-instruction "D+1")]
      (is (= [dest comp jump] [nil "D+1" nil])))))

(deftest parse-instruction-test
  (testing "parse A instruction"
    (let [{:keys [type address]} (parse-instruction "@123")]
      (is (= type "A_COMMAND"))
      (is (= address "123"))))
  (testing "parse C instruction")
    (let [{:keys [type dest comp jump]} (parse-instruction "D=1")]
      (is (= type "C_COMMAND"))
      (is (= [dest comp jump] ["D" "1" nil]))))

(run-tests)