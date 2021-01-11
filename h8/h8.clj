(defn replace-value-in-termstring
  [value replacement string]
  (clojure.string/replace
   string
   (re-pattern (str "(\\A|\\s|\\[|\\{|\\()(" value ")(\\)|\\}|\\]|\\s|\\z)"))
   (str "$1" replacement "$3")))

(defn replace-values-in-termstring
   [values replacements string]
  (if (and (seq values) (seq replacements))
    (let [[v & vs] values
          [r & rs] replacements]
      (replace-values-in-termstring vs rs
                                    (replace-value-in-termstring v r string)))
    string))

(defn closing-paren-in-string
    [s]
  (letfn [(nth-closing-paren-in-string [s openings]
            (cond
              (> 2 (count s)) `(~(apply str s) "")
              :else
              (let [[c & cs] s]
                (cond
                  (= c \() (let [[before after] (nth-closing-paren-in-string cs (+ openings 1))]
                              `(~(str c before) ~(apply str after)))
                  (= c \)) (if (= openings 0)
                             `(~(str c) ~(apply str cs))
                             (let [[before after] (nth-closing-paren-in-string cs (- openings 1))]
                                `(~(str c before) ~after)))
                  :else     (let [[before after] (nth-closing-paren-in-string cs openings)]
                              `(~(str c before) ~after))))))]
    (nth-closing-paren-in-string s 0)))

(defn replace-call-in-termstring
  [f replacement string]
   (let [m (re-find (re-pattern (str "(.*)\\((" f "\\s.*)")) string)]
     (if m
       (let [[whole before call-and-after] m
             [callbody after] (closing-paren-in-string call-and-after)]
         (str before replacement after))
       string)))

(defmacro unwindrec
  ([name args basecond basebody reccond recbody]
   `(unwindrec ~name ~args ~basecond ~basebody ~reccond ~recbody nil))
  ([name args basecond basebody reccond recbody elsebody]
   `(defn ~name ~args
      (letfn
          [(~'f [~'context ~@args]
            (cond
              ~basecond (let [~'value ~basebody]
                          (println (replace-value-in-termstring
                                    '~'rec
                                    ~'value
                                    ~'context))
                          ~'value)
              ~reccond  (let [~'this-context-with-call (replace-values-in-termstring
                                                        '~args
                                                        (map str ~args)
                                                        (replace-value-in-termstring
                                                         '~'rec
                                                         (str '~recbody)
                                                         ~'context))
                              ~'this-context-with-rec (replace-call-in-termstring
                                                       '~name
                                                       "rec"
                                                       ~'this-context-with-call)]
                          (println ~'this-context-with-call)
                          (let [~'result ~(clojure.walk/prewalk-replace {name '(partial f this-context-with-rec)} recbody)
                                ~'this-context-with-result (replace-value-in-termstring '~'rec (str ~'result) ~'context)]
                            (when (not= ~'this-context-with-result (str ~'result))
                              (println ~'this-context-with-result))
                            ~'result))
              :else ~elsebody))]
        (println (replace-values-in-termstring
                  '~args
                  (map str ~args)
                  (str "(" (clojure.string/join " " '(~name ~@args)) ")")))
        (~'f "rec" ~@args)))))





(unwindrec exponent [m n]
            (= n 0) 1
            (> n 0) (* m (exponent m (- n 1)))
            (throw (Exception. "Trying to calculate exponent of a negative number.")))


(defn exponent-tr [m n]
  (unwindrec exponent-tr-helper [m n collect]
             (= n 0) collect
             (> n 0) (exponent-tr-helper m (- n 1) (* collect m))
             (throw (Exception. "Trying to calculate exponent of a negative number.")))
  (exponent-tr-helper m n 1))

(unwindrec sumlist [l]
    (empty? l) 0
    (not (empty? l)) (+ (first l) (sumlist (rest l)))
    (throw (Exception. "Trying to add different types")))


(defn sumlist-tr [l]
    (unwindrec sumlist-tr-helper [l collect]
             (empty? l)  collect
             (not (empty? l)) (sumlist-tr-helper (rest l) (+ collect (first l)))
             (throw (Exception. "Trying to add different types")))
    (sumlist-tr-helper l 0))

(unwindrec flattenlist [l]
    (empty? l) '()
    (not (empty? l)) (concat (first l) (flattenlist (rest l)))
    (throw (Exception. "Fail to flatten")))

(defn flattenlist-tr [l]
    (unwindrec flattenlist-tr-helper [l collect]
            (empty? l) collect
            (not (empty? l)) (flattenlist-tr-helper (rest l) (concat collect (first l)))
            (throw (Exception. "Fail to flatten")))
    (flattenlist-tr-helper l '() ))

(unwindrec postfixes [l]
           (empty? l) '(())
           (not (empty? l)) (concat `(~l) (postfixes (rest l)))
           (throw (Exception. "Fail to get the postfix.")))

(defn postfixes-tr [l]
  (unwindrec postfixes-tr-helper [l collect]
             (empty? l) (concat collect '(()))
             (not (empty? l)) (postfixes-tr-helper (rest l) (concat collect `(~l)))
             (throw (Exception. "Fail to get the postfix.")))
  (postfixes-tr-helper l '()))

; (print (exponent 2 3))
(print (postfixes '(1 2 3 4 5 6 7 8)))
; (print (concat (first '(1,2,3)) (rest '(4,5))))
; (print (concat(first '((1 2) '(3 4) '(5 6 7 8))) (rest '((1 2) (3 4))) ))