; GCExpr
(defrecord GCConst [int])
(defrecord GCVar [sym])
(defrecord GCOp [a1 a2 sym])

; GCTest
(defrecord GCTrue [])
(defrecord GCFalse [])
(defrecord GCComp [a1 a2 sym])
(defrecord GCAnd [a1 a2])
(defrecord GCOr [a1 a2])

; GCStmt
(defrecord GCSkip [])
(defrecord GCAssign [sym a2])
(defrecord GCCompose [a1 a2])
(defrecord GCIf [l])
(defrecord GCDo [l])

(defrecord Config [stmt sig])

(defn emptyState [] (fn [x] 0))
(defn updateState [sigma x n] (fn [y] (if (= x y) n (sigma y))))

; inputs are an expression and an state
(defn reduce_helper [exp s]
    (let [type (type exp)]
        (cond ; similar to case in ruby 
            (= type GCConst)
                (.int exp) ; access operator by .
            (= type GCVar)
                (s (.sym exp))
            (= type GCOp)
                ; assign a1, a2, operation the corresponding value.
                (let [a1 (reduce_helper (.a1 exp) s) a2 (reduce_helper (.a2 exp) s) operator (.sym exp) ]
                    (cond 
                        (= operator :plus) (+ a1 a2)
                        (= operator :minus) (- a1 a2)
                        (= operator :times) (* a1 a2)
                        (= operator :div) (/ a1 a2)
                    )
                )
            (= type GCTrue) true
            (= type GCFalse) false
            (= type GCComp)
                (let [a1 (reduce_helper (.a1 exp) s) a2 (reduce_helper (.a2 exp) s) operator (.sym exp)]
                    (cond 
                        (= operator :eq) (+ a1 a2)
                        (= operator :less) (- a1 a2)
                        (= operator :greater) (* a1 a2)
                    )
                )
            (= type GCAnd)
                (let [a1 (reduce_helper (.a1 exp) s) a2 (reduce_helper (.a2 exp) s)]
                        (and a1 a2)
                )
            (= type GCOr)
                (let [a1 (reduce_helper (.a1 exp) s) a2 (reduce_helper (.a2 exp) s)]
                        (or a1 a2)
                )
        )             
    )
)

(defn reduce
    [config]
    ; assign values
    (let [stmt (.stmt config) state (.sig config) type (type stmt)]
        (cond
            (= type GCSkip) (Config. stmt state)
            (= type GCAssign)
                (let [name (.sym stmt) value (reduce_helper (.a2 stmt) state) stmt2 (GCSkip.) state2 (updateState state name value)] ; skip to next stmt expression, and update state
                    (Config. stmt2 state2))
            (= type GCCompose)
                (let [stmt1 (.a1 stmt) stmt2 (.a2 stmt)
                      config2 (reduce (Config. stmt1 state))
                      newStmt (if (= (.stmt config2) (GCSkip.)) stmt2 (GCCompose. (.stmt config2) stmt2))
                      newState (.sig config2)]
                    (Config. newStmt newState))
            (or (= type GCIf) (= type GCDo))
                (let [ifList (.l stmt) length (count ifList) random_element (nth ifList (rand-int length)) tests (nth random_element 0) stmts (nth random_element 1)
                     newStmt (if (reduce_helper tests state) stmts (GCSkip.))] ; assigning variables
                    (Config. newStmt state))
        )
    )
)
