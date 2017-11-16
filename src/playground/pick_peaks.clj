(ns playground.pick-peaks)

;; https://www.codewars.com/kata/pick-peaks

(defn pick-peaks-by
  ([keyfn]
   (fn [xf]
     (let [pv (volatile! [::none false])]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result input]
            (let [[prior ascending] @pv
                  ik (keyfn input)
                  pk (if (= prior ::none) (inc ik) (keyfn prior))]
              ;(println :r result :i input :p prior :a ascending)
              (if (= pk ik)
                  result
                  (do
                    (vreset! pv [input (< pk ik)])
                    (if (and ascending (> pk ik))
                        (xf result prior)
                        result)))))))))
  ([keyfn coll] (sequence (pick-peaks-by keyfn) coll)))

(defn pick-peaks
  [coll]
  (let [peaks (pick-peaks-by second (map-indexed vector coll))]
    {:pos (map first peaks)
     :peaks (map second peaks)}))
