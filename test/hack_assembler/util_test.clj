(ns hack-assembler.util-test
  (:require [clojure.test :refer :all]
            [hack-assembler.util :refer :all]))

(deftest convert-to-binary-test
    (testing "converts integer to binary string of specified width"
        (is (= "101" (convert-to-binary 5 3)))
        (is (= "001100" (convert-to-binary 12 6)))
        (is (= "000" (convert-to-binary 0 3))))
    (testing "pads output to 15 characters if width not specified"
        (is (= "000100000000011" (convert-to-binary 2051)))
     (is (= "000000000000000" (convert-to-binary 0)))))
    
    