(defrecord GuardedCommand [guard command])
(GuardedCommand. '(> x 5) '(- x 1))

; part 1
(defn allowed-commands
  [commands]
  (if (empty? commands) nil
      (let [[command & rest] commands]
        (if (eval(.guard command)) (concat `(~(.command command)) (allowed-commands rest))
            (allowed-commands rest)))))
     
; part 2     
(defmacro guarded-if
  [& commands]
  `(eval 
    (rand-nth(allowed-commands 
     [~@commands]))))

; part 3
(defmacro guarded-do
  [& commands]
     `(map
     eval (allowed-commands 
     [~@commands]))
) 

(defn compare-x-y [x y]
    [(GuardedCommand. `(= ~x 0) `(~y))
     (GuardedCommand. `(= ~y 0) `(~x))
     (GuardedCommand. `(= ~x ~y) `(~x))
     (GuardedCommand. `(and (>  ~x ~y) (not= 0 ~x)) `(compare-x-y ~y (mod ~x ~y)))
     (GuardedCommand. `(and (<  ~x ~y) (not= 0 ~y)) `(compare-x-y ~x (mod ~y ~x)))
])

(defn gcd[x y]
(guarded-if (nth (compare-x-y x y) 0) (nth (compare-x-y x y) 1) (nth (compare-x-y x y) 2)
                  (nth (compare-x-y x y) 3) (nth (compare-x-y x y) 4) )
)

