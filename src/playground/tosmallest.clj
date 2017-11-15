(ns playground.tosmallest)

;; https://www.codewars.com/kata/find-the-smallest/clojure

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn join-num [ds]
  (reduce (fn [a v] (+ (* a 10) v)) ds))

(defn remove-at [col i]
  (let [v (vec col)]
    (concat (subvec v 0 i)
            (subvec v (inc i)))))
          
(defn insert-at [col i e]
  (let [[b f] (split-at i col)]
    (concat b [e] f)))

(defn rm-ins-num [ds r i]
  (let [nds (insert-at (remove-at ds r) i (nth ds r) )]
    [(join-num nds) r i]))
    
;; min-key doens't preserv order
(defn min-key-c [k col]
  (reduce (fn [a v] (if (< (k v) (k a)) v a)) col))

;; 12345[7]6
(defn reverse-point [ids]
  (reduce (fn [a v]
            (cond
              (< (second v) (second a)) (reduced a)
              (= (second v) (second a)) a
              :else v))
          ids))

;; 123[4]445 for 4
(defn insert-point [ids h]
  (reduce (fn [a v]
            (cond
              (> (second v) h) (reduced (if (nil? a) v a))
              (= (second v) (second a) h) a
              (= (second v) h) v
              :else nil))
          nil
          ids))

;; 234112[1]14
(defn smallest-point [ids]
  (first (reduce (fn [[[_ s :as a] last] v]
                   [(cond
                      (< (second v) s) v
                      (= (second v) s last) a
                      (= (second v) s) v
                      :else a)
                    (second v)])
                 [[-1 10] 10]
                 ids)))

(defn move-reverse-point [ids rp]
  [(first rp)
   (dec (or (first (insert-point (drop (first rp) ids) (second rp)))
            (count ids)))])

(defn move-smallest [ids rp]
  (let [sp (smallest-point (drop (first rp) ids))]
    [(first sp)
     (or (first (insert-point ids (second sp)))
         (first sp))]))

(defn smallest [n]
  (let [ds (digits n)
        ids (map-indexed vector ds)
        rp (reverse-point ids)]
    (->> (map #(% ids rp) [move-reverse-point move-smallest])
         (map #(apply rm-ins-num ds %))
         (min-key-c first))))
