(ns codewars.brainfuck
  (:require [clojure.string :as str]))

;; https://www.codewars.com/kata/my-smallest-code-interpreter-aka-brainf-star-star-k

;; > increment the data pointer (to point to the next cell to the right).
;; < decrement the data pointer (to point to the next cell to the left).
;; + increment (increase by one, truncate overflow: 255 + 1 = 0) the byte at the data pointer.
;; - decrement (decrease by one, treat as unsigned byte: 0 - 1 = 255 ) the byte at the data pointer.
;; . output the byte at the data pointer.
;; , accept one byte of input, storing its value in the byte at the data pointer.
;; [ if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
;; ] if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.

; TODOs
; x use map for memory, don't need to extend-mem every time
; x pass mem dp code ip and input in parameters directly, save the getter every time
; - return output directly, use lazy-seq for output
; x use loop inside function, rather than multifunction

(defn mem-read [mem addr] (get mem addr 0))
(defn mem-write [mem addr v] (assoc mem addr v))
(defn mem-op [mem addr op] (mem-write mem addr (mod (op (mem-read mem addr)) 256)))


(defn match-jmp [code pc op]
  (loop [nb 0 pc pc]
    (let [nb' (case (get code pc)
                \[ (dec nb)
                \] (inc nb)
                nil 0       ; error, need to stop
                nb)]
      (if (zero? nb')
          (inc pc)
          (recur nb' (op pc))))))


(defn dump-mem [mem addr]
  (str/join " "
            (map #(if (= addr %)
                      (str \* (mem-read mem %) \*)
                      (mem-read mem %))
                  (range (inc (apply max (conj (keys mem) addr)))))))

(defn print-state [code pc mem addr]
  (println "mem:" (dump-mem mem addr))
  (let [s (- pc 10)
        st (if (neg? s) 0 s)
        e (+ pc 10)
        ed (if (> e (count code)) (count code) e)]
    (println "code:(" (subs code st ed) ")")
    (println "      " (apply str (conj (vec (repeat (- pc st) " ")) "^")))))


(defn execute [code pc mem addr input output]
  (print-state code pc mem addr)
  (case (get code pc)
    nil output
    \> (recur code (inc pc) mem (inc addr) input output)
    \< (recur code (inc pc) mem (dec addr) input output)
    \+ (recur code (inc pc) (mem-op mem addr inc) addr input output)
    \- (recur code (inc pc) (mem-op mem addr dec) addr input output)
    \. (recur code (inc pc) mem addr input (conj output (char (mem-read mem addr))))
    \, (when-let [c (first input)]
         (recur code (inc pc) (mem-write mem addr (int c)) addr (rest input) output))
    \[ (recur code
              (if (zero? (mem-read mem addr))
                  (match-jmp code pc inc)
                  (inc pc))
              mem addr input output)
    \] (recur code
              (if (zero? (mem-read mem addr))
                  (inc pc)
                  (match-jmp code pc dec))
              mem addr input output)
    (recur code (inc pc) mem addr input output)))


(defn execute-string
  "Evaluate the Brainfuck source code in `source` using `input` as a source of
  characters for the `,` input command.

  Either returns a sequence of output characters, or `nil` if there was
  insufficient input."
  [source input]
  (when-let [output (execute source 0 {} 0 input [])]
    (str/join "" output)))

;(execute-string ",>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"
;                "\n")
