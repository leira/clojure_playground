(ns codewars.sumbyfactors-test
  (:require [clojure.test :refer :all]
            [codewars.sumbyfactors :refer :all]))

(deftest a-test1
  (testing "Test1"
    (is (= (sum-of-divided [12, 15]) [ [2 12] [3 27] [5 15] ] ))))
