"Analyze a Tic-Tac-Toe Board"
"A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won."
(defn ttt [matrix]
  (let [[[r1c1 r1c2 r1c3]
         [r2c1 r2c2 r2c3]
         [r3c1 r3c2 r3c3]] matrix]
    (first
     (first
      (filter #(= (count %) 1)
              (filter #(not (contains? % :e))
                      (map set [[r1c1 r1c2 r1c3]
                                [r2c1 r2c2 r2c3]
                                [r3c1 r3c2 r3c3]
                                [r1c1 r2c1 r3c1]
                                [r1c2 r2c2 r3c2]
                                [r1c3 r2c3 r3c3]
                                [r1c1 r2c2 r3c3]
                                [r1c3 r2c2 r3c1]])))))))
(assert (= nil (ttt [[:e :e :e]
                     [:e :e :e]
                     [:e :e :e]])))
(assert (= :x (ttt [[:x :e :o]
                    [:x :e :e]
                    [:x :e :o]])))
(assert (= :o (ttt [[:e :x :e]
                    [:o :o :o]
                    [:x :e :x]])))
(assert (= nil (ttt [[:x :e :o]
                     [:x :x :e]
                     [:o :x :o]])))
(assert (= :x (ttt [[:x :e :e]
                    [:o :x :e]
                    [:o :e :x]])))
(assert (= :o (ttt [[:x :e :o]
                    [:x :o :e]
                    [:o :e :x]])))
(assert (= nil (ttt [[:x :o :x]
                     [:x :o :x]
                     [:o :x :o]])))

"Read Roman numerals"
"Roman numerals are easy to recognize, but not everyone knows all the rules necessary to work with them. Write a function to parse a Roman-numeral string and return the number it represents.

You can assume that the input will be well-formed, in upper-case, and follow the subtractive principle. You don't need to handle any numbers greater than MMMCMXCIX (3999), the largest number representable with ordinary letters."
(defn roman-numerals [rn]
  (let [symbols {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (let [translated (map #(get symbols %) rn)]
      (apply + (cons (last translated)
                     (map (fn [[a b]] (if (>= a b) a (- a)))
                          (partition 2 1 translated)))))))
(assert (= 14 (roman-numerals "XIV")))
(assert (= 827 (roman-numerals "DCCCXXVII")))
(assert (= 3999 (roman-numerals "MMMCMXCIX")))
(assert (= 48 (roman-numerals "XLVIII")))

"Triangle Minimal Path"
"Write a function which calculates the sum of the minimal path through a triangle. The triangle is represented as a collection of vectors. The path should start at the top of the triangle and move to an adjacent number on the next row until the bottom of the triangle is reached."
(defn min-path [tri]
  (first (reduce (fn [paths v]
                   (map #(apply + %)
                        (map vector (map #(apply min %)
                                         (partition 2 1 paths)) v)))
                 (last tri)
                 (reverse (butlast tri)))))
(assert (= 7 (min-path '([1]
                         [2 4]
                         [5 1 4]
                         [2 3 4 5])))) ; 1->2->1->3
(assert (= 20 (min-path '([3]
                          [2 4]
                          [1 9 3]
                          [9 9 2 4]
                          [4 6 6 7 8]
                          [5 7 3 5 1 4])))) ; 3->4->3->2->7->1
