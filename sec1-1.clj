;;; My solutions to exercises from Structure and Interpretation of
;;; Computer Programs.



;; Ex. 1.3
;  Maybe using SORT is not really the point of this exercise ;-)
(defn ex-1-3 [a b c]
  (let [sorted (sort > [a b c])
        fst (first sorted)
        snd (second sorted)]
    (+ (* fst fst) (* snd snd))))



;; Ex. 1.4
;  Clojure, just as Scheme, treats functions as first class objects. The
;  evaluation of '(if (> b 0) + -) returns such a function: Either + or -,
;  depending on the sign of B. Said function is then applied to A and B,
;  returning the sum of A and the absolute value of B.



;; Ex. 1.5
;  First, here is a Clojure version of P, which is semantically identical
;  to the Scheme version from the book.

(defn p [] (recur))

;  A normal-order interpreter will evaluate Ben's test to zero.
;  applicative-order evaluation, on the other hand, will lead the program
;  into an infinite loop.
;
;  In the normal-order case, the test will be expanded into the IF-form
;  before evaluating the arguments. Since the IF's condition is true, it
;  returns zero, leaving '(p) unevaluated. The applicative-order interpreter
;  will evaluate '(p) before expanding TEST, leading into an infinite loop.



;; Ex. 1.6
;  (Assuming applicative-order, or strict, evaluation) NEW-IF is an ordinary
;  function. This means, that all it's arguments are evaluated (unlike in
;  the special form IF). SQUARE-ROOT-ITER is defined recursively and thus
;  results in an infinite loop.



;; Ex. 1.7
(defn square [x]
  (* x x))

(defn abs [x]
  (if (pos? x) x (- x)))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x)
           x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

; GOOD-ENOUGH tests against the fixed value 0.001. For small numbers, close
; to this value, the ratio between the guess and the expected value becomes
; larger. For instance (sqrt 0.001) gives 0.0412 while the expected value
; is 0.0316. A difference at the most significant digit is certainly not
; acceptable.
; For large numbers SQRT _can_ end in an infinite loop. Between two large
; floating point numbers there can be a relatively large difference with
; no possible values in between. This can cause the IMPROVE function to
; return it's GUESS argument, unimproved. Example:
; (improve 3.1464265445104545E50 99E99) returns 3.1464265445104545E50
; (yes those two numbers are the same ;-)
;
; As the authors suggest, SQRT-ITER can be improved by testing for a small
; ratio between the guess and the improved guess. This way, for small
; numbers, there will be further iterations. On the other hand, if a large
; float is not further improvable, the computation stops.

(defn good-enough? [impr-guess guess]
  (let [guess-ratio (/ impr-guess guess)]
    (and (< guess-ratio 1.001)
         (> guess-ratio 0.999))))

(defn sqrt-iter [guess x]
  (let [impr-guess (improve guess x)]
    (if (good-enough? impr-guess guess)
      impr-guess
      (recur impr-guess x))))

; (sqrt 99E99) => 3.1464265788719304E50
; (sqrt 0.001) => 0.03162278245070105



;; Ex. 1.8
;  The code for this exercise would be very similar to the code above.
;  The main differences are:
;   * CUBE and CUBE-ROOT instead of SQARE and SQUARE-ROOT
;   * new implementation of IMPROVE, along the lines of the book
(defn improve [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))


