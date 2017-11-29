(ns playground.dblinear)

; https://www.codewars.com/kata/twice-linear/train/clojure

(defn dblp [x] (inc (* x 2)))
(defn tplp [x] (inc (* x 3)))

(defn gen-dbls
  ([] (lazy-seq (gen-dbls (sorted-set 1))))
  ([bases]
    (let [b (first bases)]
      (cons b 
            (lazy-seq
             (gen-dbls (conj (apply sorted-set 
                                    (rest bases))
                             (dblp b)
                             (tplp b))))))))

(defn dblinear [n]
    (last (take (inc n) (gen-dbls))))

; (dblinear 1000) => 8488
