
;; Ex. 1.29
(defn sum [term a nxt b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nxt a) nxt b))))

(defn integral [f a b dx]
  (* (sum f
          (+ a (/ dx 2))
          (fn [x] (+ x dx))
          b)
     dx))

(defn simpson-integral [f a b n]
  (assert (even? n))
  (let [h (/ (- b a) n)]
    (* (/ h 3)
       (sum (fn [k]
              (* (cond
                   (or (zero? k) (= n k)) 1
                   (even? k) 2
                   :else 4)
                 (f (+ a (* k h)))))
            0
            inc
            n))))

; The Simpson's rule-integration gives more accurate results for the same
; amount of iterations:
; (integral cube 0.0 1.0 0.01)        => 0.24998750000000042
; (simpson-integral cube 0.0 1.0 100) => 0.24999999999999992
; and
; (integral cube 0.0 1.0 0.001)        => 0.249999875000001
; (simpson-integral cube 0.0 1.0 1000) => 0.2500000000000003



;; Ex. 1.30
(defn sum [term a nxt b]
  (loop [a a res 0]
    (if (> a b)
      res
      (recur (nxt a) (+ res (term a))))))



;; Ex. 1.31
;; a. (recursive process)
(defn product [term a nxt b]
  (if (> a b)
    1
    (* (term a)
       (product term (nxt a) nxt b))))

(defn factorial [n]
  (product identity 1 inc n))

; Warning: uses ratios, which are slow
(defn approximate-pi [steps]
  (float
    (* (product (fn [k]
                  (/ (if (even? k)
                       (+ k 2)
                       (+ k 1))
                     (if (even? k)
                       (+ k 1)
                       (+ k 2))))
                1
                inc
                steps)
       4)))

;; b. (iterative process)
(defn product [term a nxt b]
  (loop [a a res 1]
    (if (> a b)
      res
      (recur (nxt a) (* res (term a))))))



;; Ex. 1.32
;; a. (recursive process)
(defn accumulate [combiner null-value term a nxt b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (nxt a) nxt b))))

(defn sum [term a nxt b]
  (accumulate + 0 term a nxt b))

(defn product [term a nxt b]
  (accumulate * 1 term a nxt b))

;; b. (iterative process)
(defn accumulate [combiner null-value term a nxt b]
  (loop [a a res null-value]
    (if (> a b)
      res
      (recur (nxt a) (combiner res (term a))))))



;; Ex. 1.33
(defn filtered-accumulate [combiner null-value term a nxt b filt]
  (loop [a a res null-value]
    (if (> a b)
      res
      (recur (nxt a)
             (combiner res
                       (if (filt a)
                         (term a)
                         null-value))))))

(defn sum-of-squares-of-primes [a b]
  (filtered-accumulate + 0 square a inc b prime?))

(defn product-of-relative-primes [n]
  (filtered-accumulate *
                       1
                       identity
                       1
                       inc
                       (dec n)
                       (fn [k] (= (gcd k n) 1))))



;; Ex. 1.34
; (f f) reduces to (2 2). This, of course, is an error since 2 is not a
; function.

