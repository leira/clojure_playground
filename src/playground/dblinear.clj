(ns playground.dblinear)

; https://www.codewars.com/kata/twice-linear/train/clojure

(defn yf [x] (inc (* x 2)))
(defn zf [x] (inc (* x 3)))

(defn gen-dbls
  ([] (cons 1 (lazy-seq (gen-dbls [1] []))))
  ([bases tbd]
    (let [x (first bases)
          y (yf x)
          z (zf x)
          ntbd (apply sorted-set (conj tbd y z))
          determined (subseq ntbd <= y)]
      (concat determined 
            (lazy-seq
             (gen-dbls (concat (rest bases) determined)
                       (subseq ntbd > y)))))))

(defn dblinear [n]
    (last (take (inc n) (gen-dbls))))

;(take 10 (gen-dbls))
(dblinear 1000)
