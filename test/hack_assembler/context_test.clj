(ns hack-assembler.context-test
    (:require [clojure.test :refer :all]
              [hack-assembler.context :refer :all]))

(deftest initialize-context-test
    (testing "Initializes context"
        (let [ctx (initialize-context {"SP" 0})]
            (is (= 1 (:line-number ctx)))
            (is (= 1 (:instruction-number ctx)))
            (is (= {"SP" 0} (:symbol-table ctx))))))

(deftest inc-instruction-test
    (testing "Increments instruction number"
        (let [ctx {:instruction-number 11 :line-number 1 :symbol-table {}}
              new-ctx (inc-instruction ctx)]
             (is (= {:instruction-number 12
                     :line-number 1
                     :symbol-table {}}
                  new-ctx)))))

(deftest inc-line-test
    (testing "Increments line number"
        (let [ctx {:instruction-number 100 :line-number 325 :symbol-table {}}
              new-ctx (inc-line ctx)]
             (is (= {:instruction-number 100
                     :line-number 326
                     :symbol-table {}} 
                    new-ctx)))))