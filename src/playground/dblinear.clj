(ns playground.dblinear)

; https://www.codewars.com/kata/twice-linear/train/clojure

(defn yf [x] (inc (* x 2)))
(defn zf [x] (inc (* x 3)))

(def gen-dbls-m
  (memoize (fn [bases tbd]
    (let [x (first bases)
          y (yf x)
          z (zf x)
          ntbd (apply sorted-set (conj tbd y z))
          determined (subseq ntbd <= y)]
      (concat determined 
            (lazy-seq
             (gen-dbls-m (concat (rest bases) determined)
                       (subseq ntbd > y))))))))

(defn gen-dbls []
  (cons 1 (lazy-seq (gen-dbls-m [1] []))))

(defn dblinear [n]
    (last (take (inc n) (gen-dbls))))

;(take 100 (gen-dbls))
(do (time (dblinear 2000))
    (time (dblinear 2000)))
