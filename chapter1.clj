;;; My solutions to exercises from Structure and Interpretation of
;;; Computer Programs.


;; Ex. 1.3
;; Maybe using SORT is not really the point of this exercise ;-)
(defn ex-1-3 [a b c]
  (let [sorted (sort > [a b c])
        fst (first sorted)
        snd (second sorted)]
    (+ (* fst fst) (* snd snd))))


;; Ex. 1.4
(comment

  Clojure, just as Scheme, treats functions as first class objects.
  The evaluation of '(if (> b 0) + -) returns such a function:
  Either + or -, depending on the sign of B. Said function is then
  applied to A and B, returning the sum of A and the absolute value
  of B.)


;; Ex. 1.5

;; a Clojure version of P, which is semantically identical to the
;; Scheme version from the book.
(defn p [] (recur))

(comment

  A normal-order interpreter will evaluate Ben's test to zero.
  Normal-order evaluation, on the other hand, will lead the
  program into an infinite loop.

  In the normal-order case, the test will be expanded into the
  IF-form before evaluating the arguments. Since the IF's condition
  is true, it returns zero, leaving '(p) unevaluated. The 
  applicative-order interpreter will evaluate '(p) before expanding
  TEST, leading into an infinite loop.)


;; Ex. 1.6
(comment

  NEW-IF is an ordinary function. This means, that all it's arguments
  are evaluated (unlike in the special form IF). SQUARE-ROOT-ITER is
  defined recursively and thus results in an infinite loop.)

