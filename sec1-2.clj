
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



;; Ex. 1.16
; As suggested by the authors, a*b^n is constant throughout the process.
(defn fast-expt-iter [base expt]
  (loop [a 1 b base n expt]
    (cond
      (zero? n) a
      (even? n) (recur a (* b b) (/ n 2))
      :else (recur (* b a) b (dec n)))))



;; Ex. 1.17
; DOUBLE is a Clojure core function, so we will call our function TWICE.
(defn twice [x] (* 2 x))
(defn halve [x] (assert (even? x)) (/ x 2))

; Using the equality a*b = 2 (a*b/2)
(defn mult [a b]
  (cond
    (zero? b) 0
    (even? b) (twice (mult a (halve b)))
    :else (+ a (mult a (dec b)))))



;; Ex. 1.18
; Using (result = a*b + acc) as the loop invariant.
; The following equalities are used for the calculation:
;   a*0 + acc = acc
;   a*b + acc = 2a * b/2 + acc
;   a*b + acc = a * (b-1) + acc + a
(defn mult-iter [a b]
  (loop [a a b b acc 0]
    (cond
      (zero? b) acc
      (even? b) (recur (twice a) (halve b) acc)
      :else (recur a (dec b) (+ a acc)))))



;; Ex. 1.19
; The transformation Tpq is given as
;   
;   Tpq(a,b) = a <- bq + aq + ap
;              b <- bp + aq.
;
; To apply T twice we use the same definiton, substituting for a and b the
; values obtained from the first application of Tpq:
;
;   Tp'q'(a,b) = a <- bpq + aqq + bqq + aqq + apq + bpq + apq + app
;                b <- bpp + aqp + bqq+ aqq + apq
;
; We restructure this into the original form:
;
;   Tp'q'(a,b) = a <- b(2pq + qq) + a(2pq + qq) + a(pp + qq)
;                b <- b(pp + qq) + a(2pq + qq)
;
; This gives the equations we can use in the procedure.
;
;   p' = pp + qq
;   q' = 2pq + qq.
;
(defn fib [n]
  (loop [a 1 b 0 p 0 q 1 cnt n]
    (cond
      (zero? cnt) b
      (even? cnt)
        (recur a
               b
               (+ (* p p) (* q q))   ; p'
               (+ (* q q) (* 2 p q)) ; q'
               (/ cnt 2))
      :else
        (recur (+ (* b q) (* a q) (* a p))
               (+ (* b p) (* a q))
               p
               q
               (dec cnt)))))



;; Ex. 1.20
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

; applicative-order evaluation
; (gcd 206 40)
; (gcd 40 (rem 206 40)) ; 1 call
; (gcd 40 6)
; (gcd 6 (rem 40 6))    ; 1 call
; (gcd 6 4)
; (gcd 4 (rem 6 4))     ; 1 call
; (gcd 4 2)
; (gcd 2 (rem 4 2))     ; 1 call
; (gcd 2 0)
; 2
;
; A total of 4 calls

; normal-order evaluation
; (gcd 206 40)
;
; (if (zero? 40)
;   206
;   (recur 40 (rem 206 40)))
;
; (if (zero? (rem 206 40))                      ; 1 call => false
;   40
;   (recur (rem 206 40) (rem 40 (rem 206 40))))
;
; (if (zero? (rem 40 (rem 206 40)))             ; 2 calls => false
;   (rem 206 40)
;   (recur (rem 40 (rem 206 40))
;          (rem (rem 206 40)
;               (rem 40 (rem 206 40)))))
;
; (if (zero? (rem (rem 206 40)
;                 (rem 40 (rem 206 40))))       ; 4 calls => false
;   (rem 40 (rem 206 40))
;   (recur (rem (rem 206 40)
;               (rem 40 (rem 206 40)))
;          (rem (rem 40 (rem 206 40))
;               (rem (rem 206 40)
;                    (rem 40 (rem 206 40))))))
;
; (if (zero? (rem (rem 40 (rem 206 40))
;                 (rem (rem 206 40)
;                      (rem 40 (rem 206 40))))) ; 7 calls => true
;   (rem (rem 206 40)
;        (rem 40 (rem 206 40)))                 ; 4 calls
;   (recur ...))
;
; 2
;
; The total number is 1+2+4+7+4 = 18 calls.



;; Ex. 1.21
(defn divides? [a b]
  (zero? (rem b a)))

(defn find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(smallest-divisor 199) ; => 199
(smallest-divisor 1999) ; => 1999
(smallest-divisor 19999) ; 7



;; Ex 1.22

(defn report-prime [n elapsed-time]
  (print " *** " (/ elapsed-time 1000)))

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime n (- (System/nanoTime) start-time))))

(defn timed-prime-test [n]
  (print n)
  (start-prime-test n (System/nanoTime))
  (newline))

(defn search-for-primes [from to]
  (if (<= from to)
    (do
      (timed-prime-test from)
      (recur (inc from) to))))

; Primes *** time for test
;
;    1009 *** 13
;    1013 *** 13
;    1019 *** 13
;   10007 *** 42
;   10009 *** 41
;   10037 *** 41
;  100003 *** 138
;  100019 *** 138
;  100043 *** 137
; 1000003 *** 433
; 1000033 *** 435
; 1000037 *** 436
;
; These values are very close to (sqrt 10) multiples for each order of
; magnitude. So on a modern (2009) computer under light load, the runtime
; is indeed proportional to the number of computational steps.



;; Ex. 1.23
(defn nxt [n]
  (if (= n 2) 3 (+ n 2)))

(defn divides? [a b]
  (zero? (rem b a)))

(defn find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
  (loop [n n test-divisor 2]
    (cond
      (> (* test-divisor test-divisor) n) n
      (divides? test-divisor n) test-divisor
      :else (recur n (nxt test-divisor)))))

;    ~1000 : 0.01 ms
;   ~10000 : 0.026 ms
;  ~100000 : 0.072 ms
; ~1000000 : 0.220 ms
; In the direct comparison these values are slightly slower than twice the
; speed (about 1.5). A likely reason is the added NXT procedure, which is
; more expensive than a simple increment.



;; Ex. 1.24
(defn square [x] (* x x))

(defn expmod [base exp m]
  (cond
    (zero? exp) 1
    (even? exp)
      (rem (square (expmod base (/ exp 2) m))
           m)
    :else
      (rem (* base (expmod base (dec exp) m))
           m)))

(defn fermat-test [n]
  (let [r (inc (rand-int (dec n)))]
    (= (expmod r n n) r)))

(defn fast-prime? [n times]
  (cond
    (zero? times) true
    (fermat-test n) (recur n (dec times))
    :else false))

; (time (fast-prime? 1009 10))   => ~0.08 ms
; (time (fast-prime? 1000003 10) => ~0.16 ms
; This is very close to the expected doubling of runtime.



;; Ex. 1.25
; From a mathematical standpoint the code is perfectly fine. However, it
; produces very large numbers (up to n^(n-1) for a test of n). This will give
; correct results in Clojure (and most other Lisps), but the handling of such
; huge numbers is very slow, compared to numbers that fit into machine words.



;; Ex. 1.26
; The original EXPMOD is a linear recursive process. By making two recursive
; calls at each step, the process becomes tree-recursive. This means the
; runtime grows exponentially with the tree-depth:
;  O(2^log(N)) = O(N)



;; Ex. 1.27
(defn fools-fermat? [n]
  (every? (fn [a] (= (expmod a n n) a))
          (range 1 n)))

(every? fools-fermat? '(561 1105 1729 2465 2821 6601)) ; => true



;; Ex. 1.28
(defn expmod [base exp m]
  (cond
    (zero? exp) 1
    (even? exp)
      (let [x (expmod base (/ exp 2) m)
            y (rem (square x) m)]
        (if (and (not= x 1) (not= x (dec m)) (= y 1))
          0
          y))
     :else
      (rem (* base (expmod base (dec exp) m))
           m)))

(defn miller-rabin [n]
  (let [r (inc (rand-int (dec n)))]
    (= (expmod r n n) r)))

(defn miller-rabin-prime?
  ([n] (miller-rabin-prime? n 10))
  ([n times] (cond
               (zero? times) true
               (miller-rabin n) (recur n (dec times))
               :else false)))

(every? miller-rabin-prime? '(3 5 19 37 101 1601)) ; => true

(some miller-rabin-prime? '(561 1105 1729 2465 2821 6601)) ; => nil

