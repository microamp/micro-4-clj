"Last Element"
"Write a function which returns the last element in a sequence."
(defn my-last [c]
  (-> c reverse first))
(assert (= (my-last [1 2 3 4 5]) 5))
(assert (= (my-last '(5 4 3)) 3))
(assert (= (my-last ["b" "c" "d"]) "d"))

"Penultimate Element"
"Write a function which returns the second to last element from a sequence."
(defn second-to-last [c]
  (-> c reverse second))
(assert (= (second-to-last (list 1 2 3 4 5)) 4))
(assert (= (second-to-last ["a" "b" "c"]) "b"))
(assert (= (second-to-last [[1 2] [3 4]]) [1 2]))

"Duplicate a Sequence"
"Write a function which duplicates each element of a sequence."
(defn duplicate-seq [c]
  (interleave c c))
(assert (= (duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
(assert (= (duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
(assert (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

"Nth Element"
"Write a function which returns the Nth element from a sequence."
(defn my-nth [coll n]
  (first (drop n coll)))
(assert (= (my-nth '(4 5 6 7) 2) 6))
(assert (= (my-nth [:a :b :c] 0) :a))
(assert (= (my-nth [1 2 3 4] 1) 2))
(assert (= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6]))

"Count a Sequence"
"Write a function which returns the total number of elements in a sequence."
(defn my-count [c]
  (reduce + (map (fn [_] 1) c)))
(assert (= (my-count '(1 2 3 3 1)) 5))
(assert (= (my-count "Hello World") 11))
(assert (= (my-count [[1 2] [3 4] [5 6]]) 3))
(assert (= (my-count '(13)) 1))
(assert (= (my-count '(:a :b :c)) 3))

"Sum It All Up"
"Write a function which returns the sum of a sequence of numbers."
(defn sum [coll]
  (reduce + coll))
(assert (= (sum [1 2 3]) 6))
(assert (= (sum (list 0 -2 5 5)) 8))
(assert (= (sum #{4 2 1}) 7))
(assert (= (sum '(0 0 -1)) -1))
(assert (= (sum '(1 10 3)) 14))

"Find the odd numbers"
"Write a function which returns only the odd numbers from a sequence."
(defn odd-only [coll]
  (filter odd? coll))
(assert (= (odd-only #{1 2 3 4 5}) '(1 3 5)))
(assert (= (odd-only [4 2 1 6]) '(1)))
(assert (= (odd-only [2 2 4 6]) '()))
(assert (= (odd-only [1 1 1 3]) '(1 1 1 3)))

"Reverse a Sequence"
"Write a function which reverses a sequence."
(defn my-reverse [c]
  (reduce (fn [reversed item] (cons item reversed))
          (list (first c))
          (rest c)))
(assert (= (my-reverse [1 2 3 4 5]) [5 4 3 2 1]))
(assert (= (my-reverse (sorted-set 5 7 2 7)) '(7 5 2)))
(assert (= (my-reverse [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))

"Palindrome Detector"
"Write a function which returns true if the given sequence is a palindrome.
Hint: \"racecar\" does not equal '(\\r \\a \\c \\e \\c \\a \\r)"
(defn palindrome? [c]
  (= (apply str c) (apply str (reverse c))))
(assert (false? (palindrome? '(1 2 3 4 5))))
(assert (true? (palindrome? "racecar")))
(assert (true? (palindrome? [:foo :bar :foo])))
(assert (true? (palindrome? '(1 1 3 3 1 1))))
(assert (false? (palindrome? '(:a :b :c))))

"Fibonacci Sequence"
"Write a function which returns the first X fibonacci numbers."
(defn fib [n]
  (take n
        (map first
             (iterate (fn [[x y]] [y (+ x y)])
                      [1 1]))))
(assert (= (fib 3) '(1 1 2)))
(assert (= (fib 6) '(1 1 2 3 5 8)))
(assert (= (fib 8) '(1 1 2 3 5 8 13 21)))

"Maximum value"
"Write a function which takes a variable number of parameters and returns the maximum value."
(defn my-max [& nums]
  (reduce (fn [x y] (if (> x y) x y)) nums))
(assert (= (my-max 1 8 3 4) 8))
(assert (= (my-max 30 20) 30))
(assert (= (my-max 45 67 11) 67))

"Get the Caps"
"Write a function which takes a string and returns a new string containing only the capital letters."
(defn caps-only [s]
  (clojure.string/join
   (filter (fn [c] (Character/isUpperCase c)) s)))
(assert (= (caps-only "HeLlO, WoRlD!") "HLOWRD"))
(assert (empty? (caps-only "nothing")))
(assert (= (caps-only "$#A(*&987Zf") "AZ"))

"Intro to some"
"The some function takes a predicate function and a collection. It returns the first logical true value of (pre
dicate x) where x is an item in the collection."
(def x 6)
(assert (= x (some #{2 7 6} [5 6 7 8])))
(assert (= x (some #(when (even? %) %) [5 6 7 8])))

"Implement range"
"Write a function which creates a list of all integers in a given range."
(defn my-range [start end]
  (take-while #(> end %)
              (iterate inc start)))
(assert (= (my-range 1 4) '(1 2 3)))
(assert (= (my-range -2 2) '(-2 -1 0 1)))
(assert (= (my-range 5 8) '(5 6 7)))

"Factorial Fun"
"Write a function which calculates factorials."
(defn factorial [n]
  (reduce * (range 1 (inc n))))
(assert (= (factorial 1) 1))
(assert (= (factorial 3) 6))
(assert (= (factorial 5) 120))
(assert (= (factorial 8) 40320))

"Interleave Two Seqs"
"Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
(defn my-interleave [c1 c2]
  (let [len (min (count c1) (count c2))]
    (mapcat vector (take len c1) (take len c2))))
(assert (= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
(assert (= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4)))
(assert (= (my-interleave [1 2 3 4] [5]) [1 5]))
(assert (= (my-interleave [30 20] [25 15]) [30 25 20 15]))

"Compress a Sequence"
"Write a function which removes consecutive duplicates from a sequence."
(defn compress [c]
  (reduce (fn [compressed item]
            (if (= (last compressed) item)
              compressed
              (conj compressed item)))
          [(first c)]
          (rest c)))
(assert (= (apply str (compress "Leeeeeerrroyyy")) "Leroy"))
(assert (= (compress [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
(assert (= (compress [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))

"Contain Yourself"
"The contains? function checks if a KEY is present in a given collection. This often leads beginner clojurians to use it incorrectly with numerically indexed collections like vectors and lists."
(def x 4)
(assert (contains? #{4 5 6} x))
(assert (contains? [1 1 1 1 1] x))
(assert (contains? {4 :a 2 :b} x))
(not (contains? '(1 2 4) x)) ; throws IllegalArgumentException
                             ; contains? not supported on type:
                             ; clojure.lang.PersistentList
                             ; clojure.lang.RT.contains (RT.java:724)
                             ; in Clojure 1.5.1

"Intro to Iterate"
"The iterate function can be used to produce an infinite lazy sequence."
(def x [1 4 7 10 13])
(= x (take 5 (iterate #(+ 3 %) 1)))

"Replicate a Sequence"
"Write a function which replicates each element of a sequence a variable number of times."
(defn replicate [coll n]
  (mapcat #(take n (repeat %)) coll))
(assert (= (replicate [1 2 3] 2) '(1 1 2 2 3 3)))
(assert (= (replicate [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
(assert (= (replicate [4 5 6] 1) '(4 5 6)))
(assert (= (replicate [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (replicate [44 33] 2) [44 44 33 33]))

"Interpose a Seq"
"Write a function which separates the items of a sequence by an arbitrary value."
(defn my-interpose [x coll]
  (butlast (interleave coll (take (count coll) (repeat x)))))
(assert (= (my-interpose 0 [1 2 3]) [1 0 2 0 3]))
(assert (= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three"))
(assert (= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))

"Pack a Sequence"
"Write a function which packs consecutive duplicates into sub-lists."
(defn pack [c]
  (reduce (fn [v item] (if (= (-> v last last) item)
                        (assoc v (dec (count v)) (conj (last v) item))
                        (conj v [item])))
          [[(first c)]]
          (rest c)))
(assert (= (pack [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
(assert (= (pack [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
(assert (= (pack [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))

"Drop Every Nth Item"
"Write a function which drops every Nth item from a sequence."
(defn drop-every-nth [coll n]
  (mapcat #(take (dec n) %)
          (partition-all n coll)))
(assert (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
(assert (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
(assert (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6]))

"Intro to Destructuring"
"Let bindings and function parameter lists support destructuring."
(= [2 4] (let [[a b c d e f g] (range)] [c e]))

"Split a sequence"
"Write a function which will split a sequence into two parts."
(defn my-split [n c]
  (vector (take n c) (drop n c)))
(assert (= (my-split 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
(assert (= (my-split 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
(assert (= (my-split 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))

"Advanced Destructuring"
"Here is an example of some more sophisticated destructuring."
(assert (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d])))

"A Half-Truth"
"Write a function which takes a variable number of booleans. Your function should return true if some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false."
(defn half-truth [& bools]
  (and (boolean (some identity bools)) (not-every? identity bools)))
(assert (= false (half-truth false false)))
(assert (= true (half-truth true false)))
(assert (= false (half-truth true)))
(assert (= true (half-truth false true false)))
(assert (= false (half-truth true true true)))
(assert (= true (half-truth true true true false)))

"Map Construction"
"Write a function which takes a vector of keys and a vector of values and constructs a map from them."
(defn make-hash-map [keys values]
  (apply hash-map (interleave keys values)))
(assert (= (make-hash-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
(assert (= (make-hash-map [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
(assert (= (make-hash-map [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))

"Greatest Common Divisor"
"Given two integers, write a function which returns the greatest common divisor."
(defn gcd [x y]
  (let [[lower higher] (if (> x y) [y x] [x y])]
    (first (filter #(zero? (mod higher %))
                   (filter #(zero? (mod lower %))
                           (iterate dec lower))))))
(assert (= (gcd 2 4) 2))
(assert (= (gcd 10 5) 5))
(assert (= (gcd 5 7) 1))
(assert (= (gcd 1023 858) 33))

"Set Intersection"
"Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common."
(defn my-intersection [x y]
  (set (filter #(contains? y %) x)))
(assert (= (my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
(assert (= (my-intersection #{0 1 2} #{3 4 5}) #{}))
(assert (= (my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))

"Re-implement Iterate"
"Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc."
(defn my-iterate [f x]
  (map (fn [i]
         (reduce (fn [v f] (f v))
                 x
                 (repeat i f)))
       (range)))
(assert (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16]))
(assert (= (take 100 (my-iterate inc 0)) (take 100 (range))))
(assert (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))

"Simple closures"
"Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. When you combine the two together, you get something very powerful called lexical closures. With these, you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later, long after the code you're running now has finished.

It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n, return a function (f x) which computes x^n. Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined."
(defn simple-closure [n]
  (fn [x]
    (int (. Math pow x n))))
(assert (= 256
           ((simple-closure 2) 16),
           ((simple-closure 8) 2)))
(assert (= [1 8 27 64] (map (simple-closure 3) [1 2 3 4])))
(assert (= [1 2 4 8 16] (map #((simple-closure %) 2) [0 1 2 3 4])))

"Product Digits"
"Write a function which multiplies two numbers and returns the result as a sequence of its digits."
(defn prod-digits [a b]
  (map #(-> % str Integer.)
       (str (* a b))))
(assert (= (prod-digits 1 1) [1]))
(assert (= (prod-digits 99 9) [8 9 1]))
(assert (= (prod-digits 999 99) [9 8 9 0 1]))

"Cartesian Product"
"Write a function which calculates the Cartesian product of two sets."
(defn cartesian [coll1 coll2]
  (set (for [item1 coll1 item2 coll2] [item1 item2])))
(assert (= (cartesian #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
           #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
             ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
             ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
(assert (= (cartesian #{1 2 3} #{4 5})
            #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
(assert (= 300 (count (cartesian (into #{} (range 10))
                                 (into #{} (range 30))))))

"Group a Sequence"
"Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s."
(defn my-group-by [f c]
  (reduce (fn [hm item]
            (let [evaled (f item)
                  v (hm evaled [])]
              (assoc hm evaled (conj v item))))
          {(f (first c)) [(first c)]}
          (rest c)))
(assert (= (my-group-by #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
(assert (= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
           {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
(assert (= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
           {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))

"Read a binary number"
"Convert a binary number, provided in the form of a string, to its numerical value."
(defn bin-read [s]
  (int
   (reduce +
           (let [len (count s)]
             (conj (map (fn [[i v]] (if (= v \0) 0 (. Math pow 2 (- len i))))
                        (map vector (iterate inc 1) (butlast s)))
                   (if (= (last s) \0) 0 1))))))
(assert (= 0     (bin-read "0")))
(assert (= 7     (bin-read "111")))
(assert (= 8     (bin-read "1000")))
(assert (= 9     (bin-read "1001")))
(assert (= 255   (bin-read "11111111")))
(assert (= 1365  (bin-read "10101010101")))
(assert (= 65535 (bin-read "1111111111111111")))

"Symmetric Difference"
"Write a function which returns the symmetric difference of two sets. The symmetric difference is the set of items belonging to one but not both of the two sets."
(defn sym-diff [set1 set2]
  (clojure.set/union (clojure.set/difference set1 set2)
                     (clojure.set/difference set2 set1)))
(assert (= (sym-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
(assert (= (sym-diff #{:a :b :c} #{}) #{:a :b :c}))
(assert (= (sym-diff #{} #{4 5 6}) #{4 5 6}))
(assert (= (sym-diff #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))

"dot product"
"Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length."
(defn dot-prod [coll1 coll2]
  (reduce + (map (fn [[item1 item2]] (* item1 item2))
                 (map vector coll1 coll2))))
(assert (= 0 (dot-prod [0 1 0] [1 0 0])))
(assert (= 3 (dot-prod [1 1 1] [1 1 1])))
(assert (= 32 (dot-prod [1 2 3] [4 5 6])))
(assert (= 256 (dot-prod [2 5 6] [100 10 1])))

"Through the Looking Class"
"Enter a value which satisfies the following:"
(let [x java.lang.Class]
  (and (= (class x) x) x))

"Infix Calculator"
"Your friend Joe is always whining about Lisps using the prefix notation for math. Show him how you could easily write a function that does math using the infix notation. Is your favorite language that flexible, Joe? Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. Assume a simple calculator that does not do precedence and instead just calculates left to right."
(defn infix [& args]
  (reduce (fn [v [op arg]] (op v arg))
          (first args)
          (partition 2 (rest args))))
(assert (= 7  (infix 2 + 5)))
(assert (= 42 (infix 38 + 48 - 2 / 2)))
(assert (= 8  (infix 10 / 2 - 1 * 2)))
(assert (= 72 (infix 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))

"Pascal's Triangle"
"Pascal's triangle is a triangle of numbers computed using the following rules:

- The first row is 1.
- Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.

Write a function which returns the nth row of Pascal's Triangle."
(defn pascal [n]
  (nth (iterate
        (fn [c] (concat (vector (first c))
                       (map #(apply +' %) (partition 2 1 c))
                       (vector (last c))))
        [1])
       (dec n)))
(assert (= (pascal 1) [1]))
(assert (= (map pascal (range 1 6))
           [     [1]
                 [1 1]
                 [1 2 1]
                 [1 3 3 1]
                 [1 4 6 4 1]]))
(assert (= (pascal 11)
           [1 10 45 120 210 252 210 120 45 10 1]))

"Indexing Sequences"
"Transform a sequence into a sequence of pairs containing the original elements along with their index."
(defn indexer [coll]
  (map vector coll (range)))
(assert (= (indexer [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
(assert (= (indexer [0 1 3]) '((0 0) (1 1) (3 2))))
(assert (= (indexer [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))

"Sum of square of digits"
"Write a function which takes a collection of integers as an argument. Return the count of how many elements are smaller than the sum of their squared component digits. For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared."
(defn sum-squared-digits [nums]
  (count (filter (fn [n]
                   (let [squared (map #(* % %)
                                      (map #(-> % str Integer.)
                                           (str n)))]
                     (< n (apply + squared))))
                 nums)))
(assert (= 8 (sum-squared-digits (range 10))))
(assert (= 19 (sum-squared-digits (range 30))))
(assert (= 50 (sum-squared-digits (range 100))))
(assert (= 50 (sum-squared-digits (range 1000))))

"To Tree, or not to Tree"
"Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child."
"(In computer science, a binary tree is a tree data structure in which each node has at most two children (referred to as the left child and the right child). In a binary tree, the degree of each node can be at most two.)"
(defn btree? [coll]
  (if (nil? coll)
    true
    (if (not (sequential? coll))
      false
      (let [root (first coll) children (rest coll)]
        (if (or (nil? root) (not (= (count children) 2)))
          false
          (and (btree? (first children))
               (btree? (second children))))))))
(assert (= (btree? '(:a (:b nil nil) nil))
           true))
(assert (= (btree? '(:a (:b nil nil)))
           false))
(assert (= (btree? [1 nil [2 [3 nil nil] [4 nil nil]]])
           true))
(assert (= (btree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
           false))
(assert (= (btree? [1 [2 [3 [4 nil nil] nil] nil] nil])
           true))
(assert (= (btree? [1 [2 [3 [4 false nil] nil] nil] nil])
           false))
(assert (= (btree? '(:a nil ()))
           false))

"Recognize Playing Cards"
"A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.

It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five and diamond queen respectively. But these forms are not convenient for programmers, so to write a card game you need some way to parse an input string into meaningful components. For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)

Write a function which converts (for example) the string \"SJ\" into a map of {:suit :spade, :rank 9}. A ten will always be represented with the single character \"T\", rather than the two characters \"10\"".
(defn card [repr]
  (let [suits {\S :spade \H :heart \D :diamond \C :club}
        ranks {\T 8 \J 9 \Q 10 \K 11 \A 12}
        suit (first repr)
        rank (last repr)]
    {:suit (suits suit)
     :rank (or (ranks rank) (- (-> rank str Integer. int) 2))}))
(assert (= {:suit :diamond :rank 10} (card "DQ")))
(assert (= {:suit :heart :rank 3} (card "H5")))
(assert (= {:suit :club :rank 12} (card "CA")))
(assert (= (range 13) (map (comp :rank card str)
                           '[S2 S3 S4 S5 S6 S7
                             S8 S9 ST SJ SQ SK SA])))

"Least Common Multiple"
"Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios."
(defn lcm [& nums]
  (letfn [(first-divisible [n1 n2]
            (let [mx (max n1 n2) mn (min n1 n2)]
              (first (filter #(zero? (mod % mn))
                             (iterate #(+ % mx) mx)))))]
    (reduce first-divisible nums)))
(assert (== (lcm 2 3) 6))
(assert (== (lcm 5 3 7) 105))
(assert (== (lcm 1/3 2/5) 2))
(assert (== (lcm 3/4 1/6) 3/2))
(assert (== (lcm 7 5/7 2 3/5) 210))

"Pascal's Trapezoid"
"Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors, where each next one is constructed from the previous following the rules used in Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3 2].

Beware of arithmetic overflow! In clojure (since version 1.3 in 2011), if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer, an exception is thrown. You can use +' to indicate that you would rather overflow into Clojure's slower, arbitrary-precision bigint."
(defn trapezoid [coll]
  (iterate
   (fn [c] (concat (vector (first c))
                  (map #(apply +' %) (partition 2 1 c))
                  (vector (last c))))
   coll))
(assert (= (second (trapezoid [2 3 2])) [2 5 5 2]))
(assert (= (take 5 (trapezoid [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
(assert (= (take 2 (trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]]))
(assert (= (take 100 (trapezoid [2 4 2])) (rest (take 101 (trapezoid [2 2])))))

"Trees into tables"
"Because Clojure's for macro allows you to \"walk\" over multiple sequences in a nested fashion, it is excellent for transforming all sorts of sequences. If you don't want a sequence as your final output (say you want a map), you are often still best-off using for, because you can produce a sequence and feed it into a map, for examaple.

For this problem, your goal is to \"flatten\" a map of hashmaps. Each key in your output map should be the \"path\"1 that you would have to take in the original map to get to a value, so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of maps: if one of the values is a map, just leave it alone.

1 That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])"
(defn tree-to-table [m]
  (apply merge
   (flatten
    (for [[k v] m]
      (for [[k2 v2] v]
        {[k k2] v2})))))
(assert (= (tree-to-table '{a {p 1, q 2}
                            b {m 3, n 4}})
           '{[a p] 1, [a q] 2
             [b m] 3, [b n] 4}))
(assert (= (tree-to-table '{[1] {a b c d}
                            [2] {q r s t u v w x}})
           '{[[1] a] b, [[1] c] d,
             [[2] q] r, [[2] s] t,
             [[2] u] v, [[2] w] x}))
(assert (= (tree-to-table '{m {1 [a b c] 3 nil}})
           '{[m 1] [a b c], [m 3] nil}))

"Beauty is Symmetry"
"Let us define a binary tree as \"symmetric\" if the left half of the tree is the mirror image of the right half of the tree. Write a predicate to determine whether or not a given binary tree is symmetric. (see To Tree, or not to Tree for a reminder on the tree representation we're using)."
(defn mirror [tree]
  (if (nil? tree)
    nil
    (let [root (first tree)
          left-child (-> tree rest first)
          right-child (-> tree rest second)]
      (conj [root]
            (mirror right-child)
            (mirror left-child)))))
(defn symmetric? [tree]
  (let [left-child (-> tree rest first)
        right-child (-> tree rest second)]
    (= (mirror left-child)
       right-child)))
(assert (= (symmetric? '(:a (:b nil nil) (:b nil nil))) true))
(assert (= (symmetric? '(:a (:b nil nil) nil)) false))
(assert (= (symmetric? '(:a (:b nil nil) (:c nil nil))) false))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
           true))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
           false))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [6 nil nil] nil]] nil]])
           false))

"Intro to Destructuring 2"
"Sequential destructuring allows you to bind symbols to parts of sequential things (vectors, lists, seqs, etc.): (let [bindings*] exprs*) Complete the bindings so all let-parts evaluate to 3."
(assert (= 3
           (let [[op arg] [+ (range 3)]] (apply op arg))
           (let [[[op arg] b] [[+ 1] 2]] (op arg b))
           (let [[op arg] [inc 2]] (op arg))))

"Pairwise Disjoint Sets"
"Given a set of sets, create a function which returns true if no two of those sets have any elements in common1 and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them.

1Such sets are usually called pairwise disjoint or mutually disjoint."
(defn pairwise-disjoint? [sets]
  (let [joined (apply clojure.set/union sets)]
    (= (count joined) (apply + (map count sets)))))
(assert (= (pairwise-disjoint? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
           true))
(assert (= (pairwise-disjoint? #{#{:a :b :c :d :e}
                                 #{:a :b :c :d}
                                 #{:a :b :c}
                                 #{:a :b}
                                 #{:a}})
           false))
(assert (= (pairwise-disjoint? #{#{[1 2 3] [4 5]}
                                 #{[1 2] [3 4 5]}
                                 #{[1] [2] 3 4 5}
                                 #{1 2 [3 4] [5]}})
           true))
(assert (= (pairwise-disjoint? #{#{'a 'b}
                                 #{'c 'd 'e}
                                 #{'f 'g 'h 'i}
                                 #{''a ''c ''f}})
           true))
(assert (= (pairwise-disjoint? #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                                 #{#{:x :y :z} #{:x :y} #{:z} #{}}
                                 #{'[:x :y :z] [:x :y] [:z] [] {}}})
           false))
(assert (= (pairwise-disjoint? #{#{(= "true") false}
                                 #{:yes :no}
                                 #{(class 1) 0}
                                 #{(symbol "true") 'false}
                                 #{(keyword "yes") ::no}
                                 #{(class '1) (int \0)}})
           false))
(assert (= (pairwise-disjoint? #{#{distinct?}
                                 #{#(-> %) #(-> %)}
                                 #{#(-> %) #(-> %) #(-> %)}
                                 #{#(-> %) #(-> %) #(-> %)}})
           true))
(assert (= (pairwise-disjoint? #{#{(#(-> *)) + (quote mapcat) #_ nil}
                                 #{'+ '* mapcat (comment mapcat)}
                                 #{(do) set contains? nil?}
                                 #{, , , #_, , empty?}})
           false))

"Flatten a Sequence"
"Write a function which flattens a sequence."
(defn my-flatten [coll]
  (reduce (fn [a b] (if (sequential? b)
                     (concat a (my-flatten b))
                     (concat a [b])))
          []
          coll))
(assert (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
(assert (= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c")))
(assert (= (my-flatten '((((:a))))) '(:a)))

"Re-implement Map"
"Map is one of the core elements of a functional programming language. Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.

Special Restrictions:
map
map-indexed
mapcat
for"
(defn my-map [f coll]
  (reductions
   (fn [a b] (f b))
   (f (first coll))
   (rest coll)))
(assert (= [3 4 5 6 7]
           (my-map inc [2 3 4 5 6])))
(assert (= (repeat 10 nil)
           (my-map (fn [_] nil) (range 10))))
(assert (= [1000000 1000001]
           (->> (my-map inc (range))
                (drop (dec 1000000))
                (take 2))))

"Comparisons"
"For any orderable data type it's possible to derive all of the basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work). Write a function that takes three arguments, a less than operator for the data and two items to compare. The function should return a keyword describing the relationship between the two items. The keywords for the relationship between x and y are as follows:

x = y → :eq
x > y → :gt
x < y → :lt"
(defn comparisons [lt a b]
  (if (lt a b)
    :lt
    (if (lt b a)
      :gt
      :eq)))
(assert (= :gt (comparisons < 5 1)))
(assert (= :eq (comparisons (fn [x y] (< (count x) (count y))) "pear" "plum")))
(assert (= :lt (comparisons (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
(assert (= :gt (comparisons > 0 2)))
