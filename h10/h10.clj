(load-file "collection.clj") 
(defn summingPairs [xs sum]
(print("dfgdfda"))
  (letfn [(summingPairsHelper [xs the_pairs]
            ;; If `xs` is empty, we're done.
            
            (if (empty? xs) the_pairs
            
                (let [[fst & rest] xs]
                
                   (recur
                   rest
                   (doall 
                    (concat the_pairs
                            (for [snd rest ;; For each `snd` in `rest`...
                                  :when (<= (+ fst snd) sum)]
                              [fst snd]))))))
           (let [first_half  (future (summingPairs (subvec xs 0 (/ (count xs) 2)) sum ))
                  second_half (future (summingPairs (subvec xs (/ (count xs) 2)) sum ))]
            (concat first_half second_half)       
            )                   
                              
                              
                              
                             
                         
  )]
    (summingPairsHelper xs [])))


    (defn middle-value [vect]
  (when-not (empty? vect)
    (vect (quot (count vect) 2))))
(print(summingPairs input 2020))
    ; (print(middle-value input))
; (print(subvec input 0 (/ (count input) 500) ))

(defn summingPairs [xs sum]
  (letfn [(summingPairsHelper [xs the_pairs]
            ;; If `xs` is empty, we're done.
            (if (empty? xs) the_pairs
                ;; Otherwise, decompose `xs` into the `fst` element
                ;; and the `rest`.
                (let [[fst & rest] xs]
                  ;; We use the `recur` form to make the recursive call.
                  ;; This ensures tail call optimisation
                  (recur
                   rest
                   ;; Concatenate `the_pairs` we have so far with the sequence
                   ;; of every `[fst snd]` where `snd` is in `rest` with
                   ;; `fst + snd <= sum`. The `doall` outside the `concat`
                   ;; forces it to be calculated immediately; without this,
                   ;; we get a (lazy) buildup of `concat`'s which may
                   ;; cause a stack overflow when looking at the result.
                   (doall 
                    (concat the_pairs
                            (for [snd rest ;; For each `snd` in `rest`...
                                  :when (<= (+ fst snd) sum)]
                              ;;... put `[fst snd]` into this sequence.
                              [fst snd])))))))]
    (summingPairsHelper xs [])))
(println (str
          "Starting at:   "
          (.getSecond (java.time.LocalDateTime/now))
          " seconds, "
          (.getNano (java.time.LocalDateTime/now))
          " nanoseconds"))
; (println (summingPairs input 2020))
(println (str
          "Ending at:     "
          (.getSecond (java.time.LocalDateTime/now))
          " seconds, "
          (.getNano (java.time.LocalDateTime/now))
          " nanoseconds"))