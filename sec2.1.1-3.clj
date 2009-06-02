
;; Ex. 2.1
(defn make-rat [n d]
  (letfn [(reduced-rat [n d]
            (let [g (gcd n d)]
              (list (/ n g) (/ d g))))]
    (if (or (and (neg? n) (neg? d))
            (and (pos? n) (neg? d)))
      (reduced-rat (- n) (- d))
      (reduced-rat n d))))

(defn numer [r]
  (first r))

(defn denom [r]
  (fnext r))



;; Ex. 2.2
; We can make use of Clojure's destructuring here, to obtain very concise
; definitions for the constructors and selectors. However, this technique
; should only be used there, to avoid breaking the abstraction barrier.
(defn average [& nums]
  (/ (apply + nums) (count nums)))

(defn make-point [x y] [x y])

(defn x-point [[x y]] x)

(defn y-point [[x y]] y)

(defn print-point [p]
  (printf "(%d,%d)\n" (x-point p) (y-point p)))

(defn make-segment [start end] [start end])

(defn start-segment [[start end]] start)

(defn end-segment [[start end]] end)

(defn midpoint-segment [s]
  (let [start (start-segment s)
        end   (end-segment s)]
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))



;; Ex. 2.3
; TODO



;; Ex. 2.4
; By using the substitution model we verify that the given expression
; reduces to x.
;   (car (cons x y))
;   (car (fn [m] (m x y)))
;   ((fn [m] (m x y)) (fn [p q] p))
;   ((fn [p q] p) x y)
;   x
(defn cdr [z]
  (z (fn [p q] q)))



;; Ex. 2.5
; Both 2 and 3 are prime numbers. This means, by prime-factorization of
; our CONS'd number, we can determine the values of A and B.
(use 'clojure.contrib.generic.math-functions)

(defn my-cons [a b]
  (* (pow 2 a) (pow 3 b)))

(defn count-factors [n fac]
  (if (zero? (rem n fac))
    (inc (count-factors (/ n fac) fac))
    0))

(defn my-car [z] (count-factors z 2))

(defn my-cdr [z] (count-factors z 3))

; Curiously, with these definitions, CONS yields a FLOAT. CAR and CDR return
; integers anyway, because their respective results are computed by
; repeated incrementing, starting from the integer zero.



;; Ex. 2.6
;; Church numerals are the representations of natural numbers under Church
;; encoding. The higher-order function that represents natural number n is
;; a function that maps any other function f to its n-fold composition. In
;; simpler terms, the "value" of the numeral is equivalent to the number of
;; times the function encapsulates x. (Wikipedia, "Church Numerals")

; Apply f zero times to x.
(def zero (fn [f] (fn [x] x)))
; same as (def zero (constantly identity))

(def one (fn [f] (fn [x] (f x))))

(def two (fn [f] (fn [x] (f (f x)))))

; apply f (n+1) times to x
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

; The a-fold application of f to the b-fold application of f to x,
; resulting in the (a+b) fold application of f to x.
(defn add [a b]
  (fn [f] (fn [x] ((a f) ((b f) x)))))

; Applies INC n times to zero, giving the integer represented by n.
(defn church-to-int [n]
  ((n inc) 0))

