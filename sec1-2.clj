
;; Ex. 1.11
(defn f-rec [n]
  (if (< n 3) n
    (+ (* 1 (f-rec (- n 1)))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

(defn f-iter [n]
  (if (< n 3) n
    (loop [a 2 b 1 c 0 cnt n]
      (if (= cnt 2) a
        (recur (+ a (* 2 b) (* 3 c))
               a
               b
               (dec cnt))))))


;; Ex. 1.12
(defn pascal [row col]
  (if (or (= row 1)
          (= col 1)
          (= row col))
    1
    (+ (pascal (dec row) (dec col))
       (pascal (dec row) col))))

