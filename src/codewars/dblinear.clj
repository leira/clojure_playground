(ns codewars.dblinear
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

; https://www.codewars.com/kata/twice-linear/train/clojure

(tufte/add-basic-println-handler! {})

(defn yf [x] (inc (* x 2)))
(defn zf [x] (inc (* x 3)))

(defn append [q coll]
  (if (seq coll)
      (recur (conj q (first coll))
      (rest coll)) q))

(defn dblinear
  ([n] (if (zero? n) 1 (dblinear (conj clojure.lang.PersistentQueue/EMPTY 1)
                                 []
                                 (dec n))))
    ([bases tpls m]
    ;(println "dblinear" bases tpls m)
    (let [x (peek bases)
          y (yf x)
          [l r] (p :split (split-with (partial > y) tpls))
          dtmd (p :dtmd (conj (vec l) y))]
      ;(println x y l r m)
      (if (> (count dtmd) m)
          (dtmd m)
          (recur (p :new-base (append (pop bases) dtmd))
                 (p :new-tpls (conj (vec (if (= (first r) y)
                                (rest r)
                                r))
                       (zf x)))
                 (- m (count dtmd)))
      ))))

;(time (map dblinear (range 10)))
(profile {} (dblinear 20000))
