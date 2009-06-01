
;; Ex. 1.40
(defn cubic [a b c]
  (fn [x] (+ (* x x x)
             (* a x x)
             (* b x)
             c)))



;; Ex. 1.41
(defn dbl [f] (fn [x] (f (f x))))

(((dbl (dbl dbl)) inc) 5) ; => 21



;; Ex. 1.42
(defn compose [f g]
  (fn [x] (f (g x))))



;; Ex. 1.43
(defn repeated [f n]
  (fn [x]
    (loop [n n res x]
      (if (zero? n)
        res
        (recur (dec n) (f res))))))

(defn repeated [f n]
  (if (zero? n)
    identity
    (comp f (repeated f (dec n)))))

(defn repeated [f n]
  (accumulate comp
              identity
              (constantly f)
              1
              inc
              n))

(defn repeated [f n] #(nth (iterate f %) n))



;; Ex. 1.44
(def dx 0.0001)

(defn average [& nums]
  (/ (apply + nums) (count nums)))

(defn smooth [f]
  (fn [x]
    (average (f (+ x dx))
             (f x)
             (f (- x dx)))))

(defn n-fold-smooth [f n]
  ((repeated smooth n) f))



;; Ex. 1.45
; Experimenting with the required number of average-damps shows, that the
; n-th root requires log_2(n) repeated applications of AVERAGE_DAMP.
(use 'clojure.contrib.generic.math-functions)

(def tolerance 0.00001)

(defn fixed-point [f initial-guess]
  (letfn [(close-enough? [a b]
            (< (abs (- a b)) tolerance))]
    (loop [guess initial-guess]
      (let [nxt (f guess)]
        (if (close-enough? guess nxt)
          nxt
          (recur nxt))))))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn log2 [x]
  (/ (log x) (log 2)))

(defn nth-root [n x]
  (fixed-point-of-transform
    (fn [y] (/ x (pow y (dec n))))
    (repeated average-damp (log2 n))
    1.0))



;; Ex. 1.46
(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (let [nxt (improve guess)]
      (if (good-enough? guess nxt)
        nxt
        (recur nxt)))))

(defn sqrt [x]
  ((iterative-improve (fn [g1 g2]
                        (< 0.999 (/ g1 g2) 1.001))
                      (fn [g]
                        (average g (/ x g))))
     1.0))

(defn fixed-point [f guess]
  ((iterative-improve (fn [x y]
                        (< (abs (- x y))
                           tolerance))
                      f)
     guess))

