"Analyze a Tic-Tac-Toe Board"
"A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won."
(defn ttt [matrix]
  (let [[[r1c1 r1c2 r1c3]
         [r2c1 r2c2 r2c3]
         [r3c1 r3c2 r3c3]] matrix]
    (first (first
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
    (let [translated (map #(symbols %) rn)]
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

"Graph Connectivity"
"Given a graph, determine whether the graph is connected. A connected graph is such that a path exists between any two given nodes.

-Your function must return true if the graph is connected and false otherwise.
-You will be given a set of tuples representing the edges of a graph. Each member of a tuple being a vertex/node in the graph.
-Each edge is undirected (can be traversed either direction)."
(defn connected? [graph]
  ((complement empty?)
   (apply
    clojure.set/intersection
    (reduce (fn [sets edge]
              (let [seted (set edge)]
                (if (every? empty? (map #(clojure.set/intersection % seted) sets))
                  (conj sets seted)
                  (set (map (fn [temp-set]
                              (if (empty? (clojure.set/intersection temp-set seted))
                                temp-set
                                (clojure.set/union temp-set seted)))
                            sets)))))
            #{(set (first graph))}
            (rest graph)))))

(assert (= true (connected? #{[:a :a]})))
(assert (= true (connected? #{[:a :b]})))
(assert (= false (connected? #{[1 2] [2 3] [3 1]
                               [4 5] [5 6] [6 4]})))
(assert (= true (connected? #{[1 2] [2 3] [3 1]
                              [4 5] [5 6] [6 4] [3 4]})))
(assert (= false (connected? #{[:a :b] [:b :c] [:c :d]
                               [:x :y] [:d :a] [:b :e]})))
(assert (= true (connected? #{[:a :b] [:b :c] [:c :d]
                              [:x :y] [:d :a] [:b :e] [:x :a]})))

"Number Maze"
"Given a pair of numbers, the start and end point, find a path between the two using only three possible operations:

double
halve (odd numbers cannot be halved)
add 2

Find the shortest path through the \"maze\". Because there are multiple shortest paths, you must return the length of the shortest path, not the path itself."
(defn maze [start end]
  (let [funcs [(fn [x] (* x 2))
               (fn [x] (if (even? x) (/ x 2)))
               (fn [x] (+ x 2))]]
    (letfn [(apply-funcs [n]
              (filter (complement nil?)
                      (map #(% n) funcs)))]
      (inc (count
            (take-while (fn [nums] (not-any? #(= % end) nums))
                        (iterate #(mapcat apply-funcs %)
                                 [start])))))))

(assert (= 1 (maze 1 1))) ; 1
(assert (= 3 (maze 3 12))) ; 3 6 12
(assert (= 3 (maze 12 3))) ; 12 6 3
(assert (= 3 (maze 5 9))) ; 5 7 9
(assert (= 9 (maze 9 2))) ; 9 18 20 10 12 6 8 4 2
(assert (= 5 (maze 9 12))) ; 9 11 22 24 12

"Word Chains"
"A word chain consists of a set of words ordered so that each word differs by only one letter from the words directly before and after it. The one letter difference can be either an insertion, a deletion, or a substitution. Here is an example word chain:

cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog

Write a function which takes a sequence of words, and returns true if they can be arranged into one continous word chain, and false if they cannot."
(defn diff-by-one? [w1 w2]
  (let [by-len (reverse (sort-by count [w1 w2]))]
    (let [longer (first by-len) shorter (second by-len)]
      (let [first-diff (.indexOf (map not= longer shorter) true)]
        (case (- (count longer) (count shorter))
          0 (and (not= first-diff -1)
                 (= (drop (inc first-diff) longer)
                    (drop (inc first-diff) shorter)))
          1 (or (.contains longer shorter)
                (= (drop (inc first-diff) longer)
                   (drop first-diff shorter)))
          false)))))

(defn diffs [w words]
  (filter #(diff-by-one? % w) (disj words w)))

(defn chain [words]
  (let [all-diffs (into {} (map (fn [w] [w (diffs w words)]) words))]
    (let [w-least-diffs (map first (-> (group-by #(-> % val count) all-diffs)
                                       sort
                                       first
                                       second))]
      (= (apply max
                (map count
                     (last (take-while
                            (complement empty?)
                            (iterate
                             (fn [seqs] (mapcat
                                        (fn [s]
                                          (filter #(apply distinct? %)
                                                  (map #(cons % s)
                                                       (all-diffs (first s)))))
                                        seqs))
                             (map list w-least-diffs))))))
         (count words)))))

(assert (= true (chain #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
(assert (= false (chain #{"cot" "hot" "bat" "fat"})))
(assert (= false (chain #{"to" "top" "stop" "tops" "toss"})))
(assert (= true (chain #{"spout" "do" "pot" "pout" "spot" "dot"})))
(assert (= true (chain #{"share" "hares" "shares" "hare" "are"})))
(assert (= false (chain #{"share" "hares" "hare" "are"})))
