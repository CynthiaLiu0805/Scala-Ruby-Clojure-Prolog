(load-file "collection.clj") 

(defn summingPairs [xs sum]
(print("dfgdfda"))
  (letfn [(summingPairsHelper [xs the_pairs]
            (if (empty? xs) the_pairs
            
                (let [[fst & rest] xs]
                
                   (recur
                   rest
                   (doall 
                    (concat the_pairs
                            (for [snd rest 
                                  :when (<= (+ fst snd) sum)]
                              [fst snd]))))))
           (let [first_half  (future (summingPairs (subvec xs 0 (/ (count xs) 2)) sum ))
                  second_half (future (summingPairs (subvec xs (/ (count xs) 2)) sum ))]
            (concat first_half second_half)       
            )                   
  )]
    (summingPairsHelper xs [])))

