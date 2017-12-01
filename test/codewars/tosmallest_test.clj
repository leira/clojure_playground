(ns codewars.tosmallest-test
  (:require [clojure.test :refer :all]
            [codewars.tosmallest :refer :all]))

(defn dotest [n ans]
  (is (= ans (smallest n))))
(deftest a-test1
  (println "Basic Tests smallest")      
    (dotest 1000000 [1, 0, 6])
    (dotest 4000211 [2114, 0, 6])
    (dotest 2000122 [1222, 0, 4])
    (dotest 1000211 [1211, 0, 3])
    (dotest 209917 [29917, 0, 1])
    (dotest 1234576 [1234567, 5, 6])
    
    (dotest 261235 [126235, 2, 0])
    (dotest 285365 [238565, 3, 1])
    (dotest 269045 [26945, 3, 0])
    (dotest 296837 [239687, 4, 1])
    (dotest 187863002809 [18786300289 10 0])
    (dotest 1878630028009 [187863002809 10 0])
    (dotest 935855753 [358557539 0 8])
    (dotest 635655753 [356556753 0 5])
)
