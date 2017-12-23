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

(defn extend-mem [mem addr]
  (if (>= addr (count mem))
             (into mem (repeat (inc (- addr (count mem))) 0))
             mem))

(defn read-mem [mem addr] (get mem addr 0))
(defn write-mem [mem addr v] (assoc (extend-mem mem addr) addr v))


(defn update-mem [state f]
  (assoc state
         :mem
         (write-mem (:mem state)
                    (:addr state)
                    (f (read-mem (:mem state) (:addr state))))))

(defn next-cmd [state]
  (update state :pc inc))

(defn match-jump-fwd
  ([code pc] (match-jump-fwd code (inc pc) 0))
  ([code pc lbcount]
     (if (>= pc (count code))
         (throw (Exception. "No Matching Jump Point"))
         (case (get code pc)
           \] (if (zero? lbcount)
                   pc
                   (recur code (inc pc) (dec lbcount)))
           \[ (recur code (inc pc) (inc lbcount))
           (recur code (inc pc) lbcount)))))

(defn match-jump-back
  ([code pc] (match-jump-back code (dec pc) 0))
  ([code pc rbcount]
     (if (< pc 0)
         (throw (Exception. "No Matching Jump Point"))
         (case (get code pc)
           \[ (if (zero? rbcount)
                   pc
                   (recur code (dec pc) (dec rbcount)))
           \] (recur code (dec pc) (inc rbcount))
           (recur code (dec pc) rbcount)))))


;; commands
(defn move-right [state]
  (next-cmd (update state :addr inc)))

(defn move-left [state]
  (next-cmd (update state :addr #(if (pos? %) (dec %) %))))

(defn increase [state]
  (next-cmd (update-mem state #(mod (inc %) 256))))

(defn decrease [state]
  (next-cmd (update-mem state #(mod (dec %) 256))))

(defn output [state]
  (next-cmd (update state
                    :output
                    conj
                    (char (read-mem (:mem state)
                                    (:addr state))))))

(defn input [state]
  (next-cmd (assoc state
                   :mem
                   (write-mem (:mem state)
                              (:addr state)
                              (int (if (empty? (:input state))
                                       (throw (Exception. "No More Input"))
                                       (first (:input state)))))
                   :input (rest (:input state)))))

(defn jmp-fwd [state]
  (if (zero? (read-mem (:mem state)
                       (:addr state)))
      (assoc state
             :pc
             (match-jump-fwd (:code state) (:pc state)))
      (next-cmd state)))

(defn jmp-back [state]
  (if (zero? (read-mem (:mem state)
                       (:addr state)))
      (next-cmd state)
      (assoc state
             :pc
             (match-jump-back (:code state) (:pc state)))))


(defn print-state [state]
  (println "mem:" (assoc (extend-mem (:mem state) (:addr state))
                         (:addr state)
                         [(read-mem (:mem state) (:addr state))]))
  (let [s (- (:pc state) 10)
        st (if (neg? s) 0 s)
        e (+ (:pc state) 10)
        ed (if (> e (count (:code state))) (count (:code state)) e)]
    (println "code:(" (subs (:code state) st ed) ")")
    (println "      " (apply str (conj (vec (repeat (- (:pc state) st) " ")) "^"))))
  (println "output:" (:output state)))


(defn execute [state]
;  (print-state state)
  (if (>= (:pc state) (count (:code state)))
      (:output state)
      (recur ((case (get (:code state) (:pc state))
                    \> move-right
                    \< move-left
                    \+ increase
                    \- decrease
                    \. output
                    \, input
                    \[ jmp-fwd
                    \] jmp-back
                    (throw (Exception. "Invalid Code")))
              state))))

(defn execute-string
  "Evaluate the Brainfuck source code in `source` using `input` as a source of
  characters for the `,` input command.

  Either returns a sequence of output characters, or `nil` if there was
  insufficient input."
  [source input]
  (try
   (str/join "" (execute {:code source
                          :pc 0
                          :mem [0]
                          :addr 0
                          :input input
                          :output []}))
   (catch Exception e
          (println (str "caught exception: " (.getMessage e)))
          nil)))

;(execute-string ",>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"
;                "\n")
