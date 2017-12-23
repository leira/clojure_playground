(ns codewars.rainfall
  (:require [clojure.string :as str]))

;; https://www.codewars.com/kata/rainfall

(defn parse-ln [ln]
  (let [[town hist] (str/split ln #":")]
    [town
      (->> (str/split hist #",")
          (map #(second (str/split % #" ")))
          (map read-string))]))

(defn parse-data [data]
  (apply hash-map
         (mapcat parse-ln
                 (str/split-lines data))))
                 
(defn mean* [nums]                 
  (/ (apply + nums) (count nums)))
  
(defn variance* [nums]
  (/ (apply + (map #(* % %) nums)) (count nums)))

(defn apply-town [fn twn strng]
  (if-let [nums (get (parse-data strng) twn)]
    (fn nums)
    -1))

(defn mean [twn strng]
  (apply-town mean* twn strng))
  
(defn variance [twn strng] 
  (apply-town variance* twn strng)) 
