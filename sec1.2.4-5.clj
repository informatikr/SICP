
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

