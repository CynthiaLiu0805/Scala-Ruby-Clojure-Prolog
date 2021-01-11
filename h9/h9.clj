(defrecord GuardedCommand [guard command])
(GuardedCommand. '(> x 5) '(- x 1))
(defn first-allowed-command
  "Find the first command in a sequence of guarded `commands`
whose `.guard` evaluates to a truthy value and return its `.command`.
Returns `nil` if none of the guards are satisfied."
  [commands]
  ;; If the `commands` list is empty, "do nothing" by returning `nil`.
  (if (empty? commands) nil
      ;; Otherwise, deconstruct the `commands` list into
      ;; the first `command` and the `rest`.
      (let [[command & rest] commands]
        ;; Diagnostic print statement, if needed.
        ;(printf "Checking command %s with guard %s and command %s\n" command (.guard command) (.command command))
        ;; Now check the `guard`, and if it's satisfied, return the first `command`.
        (if (eval (.guard command)) (.command command)
            ;; Otherwise, continue to check the `rest` of the guarded commands.
            (first-allowed-command rest)))))

(defmacro guarded-deterministic-if
  "Given a sequence of `GuardedCommands`, `commands`,
select the first guarded command whose `.guard` evaluates
to a truthy value and evaluate its `.command`."
  [& commands]
  ;; The body must be quoted, so that nothing is evaluated until runtime.
  `(eval ;; Evaluate...
    (first-allowed-command ;; ...the command returned by first-allowed-command... 
     [~@commands]))) ;; to which we pass a vector of the commands.
;; The ~@ applied to `commands` here "splices" the elements of `commands` into place here.
;; That is, each element of the sequence `commands` is inserted here in order.
;; But not literally as a sequence (between parentheses or brackets.) Hence we wrap in [].
;; The use of the [] is actually quite particular; using a quoted list, '(...), would not work.
;; Because the guarded commands within would be treated as sequences instead of records.

; part 1
(defn allowed-commands
  [commands]
  (if (empty? commands) nil
      (let [[command & rest] commands]
        (if (eval(.guard command)) (concat `(~(.command command)) (allowed-commands rest))
            (allowed-commands rest)))))
     



(defmacro guarded-if
  "Given a sequence of `GuardedCommands`, `commands`,
select the first guarded command whose `.guard` evaluates
to a truthy value and evaluate its `.command`."
  [& commands]
  ;; The body must be quoted, so that nothing is evaluated until runtime.
  `(eval ;; Evaluate...
    (rand-nth(allowed-commands ;; ...the command returned by first-allowed-command... 
     [~@commands]))))

(defmacro guarded-do
  [& commands]
     `(map
     eval (allowed-commands 
     [~@commands]))
)

; (defmacro gcd [& x y]
  
;   `(eval(allowed-commands
;    [(GuardedCommand. `(= ~@x 0) y)
;     ; (GuardedCommand. `(= ~y 0) ~x)
;     ; (GuardedCommand. `(> ~x ~y) (gcd ~y (mod ~x ~y)))
;     ; (GuardedCommand. `(=  ~x ~y) x)
;     ; (GuardedCommand. `(< ~x ~y) (gcd ~x (mod ~y ~x)))
;     ]
;     )))

; (print (gcd 0 25))

; (defmacro gcdd
;   "Given a sequence of `GuardedCommands`, `commands`,
; select the first guarded command whose `.guard` evaluates
; to a truthy value and evaluate its `.command`."
;   [& x y]
;   ;; The body must be quoted, so that nothing is evaluated until runtime.
;   ;; Evaluate...
;   ; (print(allowed-command [~@commands]))
;      (allowed-commands 
;      [(GuardedCommand. `(= ~x 0) ~y)
;     ]))

; (print (gcdd 0 25))

; (print(list '(1 2 3) '(4,5)))

; (print(let [x 10
;       y 10]
;   (allowed-commands
;    [(GuardedCommand. `(>= ~x ~y) `(printf "%s is greater than or equal to %s " ~x ~y))
;     (GuardedCommand. `(=  ~x ~y) `(printf "%s is equal to %s " ~x ~y))
;     (GuardedCommand. `(<= ~x ~y) `(printf "%s is less than or equal to %s " ~x ~y))])))

(def compare-10-10
  (let [x 10
        y 10]
    [(GuardedCommand. `(>= ~x ~y) (str x " is >= " y))
     (GuardedCommand. `(=  ~x ~y) (str x " is =  " y))
     (GuardedCommand. `(<= ~x ~y) (str x " is <= " y))]))
; (is (some #(= "10 is >= 10" %) (allowed-commands compare-10-10))


(defmacro gd
  [x y]
     `(map eval (allowed-commands 
     [(GuardedCommand. `(> ~x ~y) 0)
            (GuardedCommand. `(= 5 5) 1)
            (GuardedCommand. `(< 5 5) 2)]
            )))


; (print(gd 7 5))
(defmacro mmax [x y]
  (allowed-commands 
   ;; For variables to maintain their meaning within a quoted list,
   ;; use the special backtick ` quote and unquote the variables with ~.
   [(GuardedCommand. `(= ~x 0) 0)
   (GuardedCommand. `(= ~y 0) 0)
   (GuardedCommand. `(= ~x ~y) `(~x))
   (GuardedCommand. `(> ~x ~y) `(mmax ~y (mod ~x ~y)))]))

; (print(mmax 25 10))
; (print(= 0 (mmax 0 10)))



; (defn gcd [x y]
;   (allowed-commands
;     [(GuardedCommand. `(= ~x 0) `(~y))
;      (GuardedCommand. `(= ~y 0) `(~x))
;      (GuardedCommand. `(= ~x ~y) `(~x))
;      (GuardedCommand. `(and (>  ~x ~y) (not= 0 ~x)) `(eval gcd-helper ~y (mod ~x ~y)))
;      (GuardedCommand. `(and (<  ~x ~y) (not= 0 ~y)) `(eval gcd-helper ~x (mod ~y ~x)))
; ]))

; (defn g[x y]
; (guarded-if compare-10-10 x y))
; (print (compare-10-10 2 3))    
; (print(allowed-commands (compare-10-10 2 3)))
; (print (gcd 8 2))

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

(print(gcd 2 2))