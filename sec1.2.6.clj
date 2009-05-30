

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

