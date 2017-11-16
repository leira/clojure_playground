(ns playground.pick-peaks)

;; https://www.codewars.com/kata/pick-peaks

(defn pick-peaks
  [coll]
  (let [pos (->> coll
                 (partition 2 1)
                 (map-indexed #(let [[p, v] %2]
                                 (if (= p v)
                                     -1
                                     (inc %1))))
                 (filter #(not= % -1))
                 (partition 3 1)
                 (filter #(let [[p v n] (map (partial nth coll) %)]
                            (and (< p v)
                                 (> v n))))
                 (map second))]
    {:pos pos, :peaks (map (partial nth coll) pos)}))
