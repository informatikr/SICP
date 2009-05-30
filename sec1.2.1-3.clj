
;; Ex. 1.9
; Clojure doesn't like to redefine +, so I will call the functions in
; question ADD.

(defn add [a b]
  (if (= a 0)
    b
    (inc (add (dec a) b))))

; This function generates a recursive process:
; (add 4 5)
; (inc (add 3 5))
; (inc (inc (add 2 5)))
; (inc (inc (inc (add 1 5))))
; (inc (inc (inc (inc (add 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(defn add [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

; The iterative process:
; (add 4 5)
; (add 3 6)
; (add 2 7)
; (add 1 8)
; (add 0 9)
; 9



;; Ex. 1.10
(defn A [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (dec x)
             (A x (dec y)))))

; (A 1 10) => 1024
; (A 2 4) => 65536
; (A 3 3) => 65536

(defn f [n] (A 0 n)) ; 2n

(defn g [n] (A 1 n)) ; 2^n

(defn h [n] (A 2 n)) ; 2^2^... (n times)



;; Ex. 1.11
; The recursive function is straightforward:
(defn f-rec [n]
  (if (< n 3) n
    (+ (* 1 (f-rec (- n 1)))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

; The iterative function requires state variables. The idea is to calculate
; the function results for all natural numbers up to N. The three most
; recent results are stored in the state variables A, B and C. At CNT = N,
; the variable A holds the result.
(defn f-iter [n]
  (if (< n 3) n
    (loop [a 2 b 1 c 0 cnt 2]
      (if (= cnt n) a
        (recur (+ a (* 2 b) (* 3 c))
               a
               b
               (inc cnt))))))



;; Ex. 1.12
; Rows and columns are numbered, starting from one. The arguments ROW and
; COL should be positive integers.
(defn pascal [row col]
  (if (or (= row 1)
          (= col 1)
          (= row col))
    1
    (+ (pascal (dec row) (dec col))
       (pascal (dec row) col))))



;; Ex. 1.13
; First, I will follow the hint and prove by induction that
; 
; (1) Fib(n) = (phi^n - psi^n) / (sqrt 5).
;
; Base cases:
;   Fib(0) = (phi^0 - psi^0) / (sqrt 5) = 0
;   Fib(1) = (phi - psi) / (sqrt 5)
;          = ( 1/2 + (sqrt 5)/2 - 1/2 + (sqrt 5)/2 ) / (sqrt 5)
;          = (sqrt 5) / (sqrt 5) = 1
;
; Inductive step:
; Assuming that (1) is true for n-1 and n-2.
;   Fib(n) = Fib(n-1) + Fib(n-2)
;          = ( phi^(n-1) - psi^(n-1) + phi^(n-2) - psi^(n-2) ) / (sqrt 5)
;          = ( (phi+1)phi^(n-2) - (psi+1)psi^(n-2) ) / (sqrt 5)
; 
; The book gives the equality phi^2 = phi+1. Similarly, it is also true that
; psi^2 = psi+1 (proof skipped). Substituting phi+1 and psi+1 gives
;
;          = ( phi^2 phi^(n-2) - psi^2 psi^(n-2) ) / (sqrt 5)
;          = ( phi^n - psi^n ) / (sqrt 5)
; 
; QED
;
; Second, for the final prove, we see that the absolute value of psi is less
; than one. Thus, for large values of n, psi^n approaches zero. This gives
; the final equation:
;
;   Fib(n) = phi^n / (sqrt 5)
;
; QED



;; Ex. 1.14
; TODO



;; Ex. 1.15
(defn cube [x] (* x x x))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (<= (abs angle) 0.1)
    angle
    (do
      (prn angle)
      (p (sine (/ angle 3.0))))))

; For an angle of 12.5, P is called five times. With each call, the angle is
; divided by three. The values of ANGLE are (roughly)
;  12.15,
;  4.05,
;  1.35,
;  0.45,
;  0.15,
;  0.05.
; The first five of these are greater than 0.1 and cause a call to P.
;
; Each call to P requires a constant amount of space and time. The number
; of calls to P is proportional to log_3(angle). The overall time- and
; space-complexity is therefore O(log n).

