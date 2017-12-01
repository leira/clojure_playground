(ns codewars.dblinear)

; https://www.codewars.com/kata/twice-linear/train/clojure

(defn yf [x] (inc (* x 2)))
(defn zf [x] (inc (* x 3)))

(defn split-before
  ([coll y m] (split-before [] coll y m))
  ([before coll y m]
    ;(println "split-before" before coll y m)
    (if (zero? m)
        [before coll 0]
        (let [n (first coll)]
          (cond (or (nil? n) (> n y))
                  [(conj before y) coll (dec m)]
                (= n y) [(conj before y) (rest coll) (dec m)]
                :esle (recur (conj before (first coll))
                             (rest coll)
                             y
                             (dec m)))))))

(defn dblinear
  ([n] (if (zero? n) 1 (dblinear [1] [] n)))
  ([bases tpls m]
    ;(println bases tpls m)
    (let [x (first bases)
          y (yf x)
          [dtmed ntpls nm] (split-before tpls y m)]
      (if (zero? nm)
          (peek dtmed)
          (recur (into (vec (rest bases)) dtmed)
                 (conj (vec ntpls) (zf x))
                 nm)))))
;  (last (take (inc n) (gen-dbls))))

;(split-before [3 5 9 19] 10 3)
;(split-before [] 3 1000)

;(time (map dblinear (range 100)))
(time (dblinear 2000))
