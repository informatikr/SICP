
;; Ex. 1.35
; The golden ratio is defined as the number phi, which satifies the equation
; phi^2 = phi + 1. By dividing both sides by phi we obtain the equivalent
; equation phi = 1 + 1/phi, which is the result of substituting phi for x
; in x -> 1 + 1/x. Thus, phi is indeed a fixed point of the given
; transformation.

(def tolerance 0.00001)

(defn fixed-point [f initial-guess]
  (letfn [(close-enough? [a b]
            (< (abs (- a b)) tolerance))]
    (loop [guess initial-guess]
      (let [nxt (f guess)]
        (if (close-enough? guess nxt)
          nxt
          (recur nxt))))))

(def phi (fixed-point #(inc (/ %)) 1.0))



;; Ex. 1.36
(use 'clojure.contrib.generic.math-functions)

(defn fixed-point [f initial-guess]
  (letfn [(close-enough? [a b]
            (< (abs (- a b)) tolerance))]
    (loop [guess initial-guess]
      (prn guess)
      (let [nxt (f guess)]
        (if (close-enough? guess nxt)
          nxt
          (recur nxt))))))

(defn average [a b] (/ (+ a b) 2))

(defn fun [x]
  (/ (log 1000) (log x)))

(defn dampened-fun [x]
  (average x (fun x)))

(fixed-point fun 2)          ; 35 steps
(fixed-point dampened-fun 2) ; 10 steps



;; Ex. 1.37
;; a.
(defn cont-frac [n d k]
  (letfn [(frac [i]
            (if (> i k)
              0
              (/ (n i)
                 (+ (d i)
                    (frac (inc i))))))]
    (frac 1)))

; It takes 11 steps to reach 4-digit accuracy on 1/phi.
(cont-frac (constantly 1.0) (constantly 1.0) 11) ; => 0.6180

;; b.
; The iterative procedure starts with the base case and works it's way up, 
; decrementing k down to the outer fraction at k = 1.
(defn cont-frac [n d k]
  (loop [k k res 0]
    (if (zero? k)
      res
      (recur (dec k)
             (/ (n k)
                (+ (d k) res))))))



;; Ex. 1.38
(def e
  (+ (cont-frac (constantly 1.0)
                (fn [k]
                  (if (zero? (mod (inc k) 3))
                    (* 2.0 (/ (inc k) 3))
                    1.0))
                10)
     2))



;; Ex. 1.39
(defn tan-cf [x k]
  (cont-frac (fn [i]
               (if (= i 1)
                 x
                 (- (* x x))))
             #(dec (* 2 %))
             k))
