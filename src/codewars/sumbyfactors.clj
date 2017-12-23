(ns codewars.sumbyfactors)

;; https://www.codewars.com/kata/sum-by-factors/clojure

(def prime-numbers
  ((fn f [x]
     (cons x
           (lazy-seq
            (f (first
                (drop-while
                 (fn [n]
                   (some #(zero? (mod n %))
                         (take-while #(<= (* % %) n) prime-numbers)))
                 (iterate inc (inc x))))))))
   2))

(defn sum-of-divided [lst]
  (let [m (apply max (map #(Math/abs %) lst))]
    (->> (take-while #(<= % m) prime-numbers)
         (map (fn [p]
                (->> (filter #(zero? (mod % p)) lst)
                     (#(if (empty? %) nil (apply + %)))
                     (vector p))))
         (filter (comp some? second)))))

(sum-of-divided [-29804 -4209 -28265 -72769 -31744])
