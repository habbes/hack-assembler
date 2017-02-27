(ns hack-assembler.symbol-table-test
    (:require [clojure.test :refer :all]
              [hack-assembler.symbol-table :refer :all]))
              

(deftest initialize-test
    (testing "Initializes symbol table with predefined symbols and var address 16"
        (let [table (initialize-table)]
            (is (= 16 (:cur-var-address table)))
            (is (= 0 (get table "SP")))
            (is (= 1 (get table "LCL")))
            (is (= 2 (get table "ARG")))
            (is (= 3 (get table "THIS")))
            (is (= 4 (get table "THAT")))
            (dotimes [n 16]
             (is (= n (get table (str "R" n))))))))

(deftest add-variable-test
    (testing "Adds variable and updates address"
        (let [table (initialize-table)]
            (let [updated (add-variable table "i")]
                (is (= 16 (get updated "i")))
                (is (= 17 (:cur-var-address updated)))
                (let [updated (add-variable updated "myvar")]
                    (is (= 17 (get updated "myvar")))
                    (is (= 18 (:cur-var-address updated)))))))) 

(deftest resolve-variable-test
    (testing "Resolves existing variables"
        (let [table (initialize-table)
              table (add-variable table "i")
              [table val] (resolve-variable table "i")]
             (is (= 16 val))
             (is (= 17 (:cur-var-address table)))))
    (testing "Adds then resolves non-existing variable"
        (let [table (initialize-table)
              [table val] (resolve-variable table "i")]
             (is (= 16 val))
             (is (= 17 (:cur-var-address table))))))

(deftest resolve-symbol-test
    (testing "Resolves existing symbols"
        (let [table {:cur-var-address 21 "A" 0 "B" 1 "i" 20}]
            (is (= [table 1] (resolve-symbol table "B")))
            (is (= [table 20] (resolve-symbol table "i")))
            (is (= 21 (:cur-var-address (nth (resolve-symbol table "i") 0))))))
    (testing "Resolves non-existing variables and updates table"
        (let [table {:cur-var-address 16} 
              [table val] (resolve-symbol table "i")]
             (is (= val 16))
             (is (= table {:cur-var-address 17 "i" 16}))))) 
