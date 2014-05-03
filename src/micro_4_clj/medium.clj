"Rotate Sequence"
"Write a function which can rotate a sequence in either direction."
(defn rotate [i coll]
  (if (>= i 0)
    (take (count coll) (drop i (cycle coll)))
    (let [limit (first (filter #(> % (Math/abs i))
                               (map #(* (count coll) %)
                                    (rest (range)))))]
      (take (count coll) (drop (- limit (Math/abs i)) (cycle coll))))))
(assert (= (rotate 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(assert (= (rotate -2 [1 2 3 4 5]) '(4 5 1 2 3)))
(assert (= (rotate 6 [1 2 3 4 5]) '(2 3 4 5 1)))
(assert (= (rotate 1 '(:a :b :c)) '(:b :c :a)))
(assert (= (rotate -4 '(:a :b :c)) '(:c :a :b)))
