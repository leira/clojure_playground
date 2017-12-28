(ns codewars.pyramid-slide-down)

; https://www.codewars.com/kata/pyramid-slide-down/

(defn longest-slide-down [pyramid]
  (first
   (reduce (fn [lsd row]
             (map (fn [l r c] (+ c (max l r))) lsd (rest lsd) row))
           (rseq pyramid))))
