(ns codewars.sumofcubes)

;; https://www.codewars.com/kata/build-a-pile-of-cubes/

(defn- guess-n [m]
  (bigint (Math/sqrt (* (Math/sqrt m) 2))))

(defn- sum-cubes [n]
  (let [x (/ (* n (+ n 1)) 2)]
    (* x x)))

(defn find-nb [m]
  (let [n (guess-n m)]
    (if (= (sum-cubes n) m)
        n
        -1)))
