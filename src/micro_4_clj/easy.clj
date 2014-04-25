"Last Element"
"Write a function which returns the last element in a sequence."
(def x #(-> % reverse first))
(assert (= (x [1 2 3 4 5]) 5))
(assert (= (x '(5 4 3)) 3))
(assert (= (x ["b" "c" "d"]) "d"))

"Penultimate Element"
"Write a function which returns the second to last element from a sequence."
(def x #(-> % reverse second))
(assert (= (x (list 1 2 3 4 5)) 4))
(assert (= (x ["a" "b" "c"]) "b"))
(assert (= (x [[1 2] [3 4]]) [1 2]))

"Duplicate a Sequence"
"Write a function which duplicates each element of a sequence."
(def x #(interleave % %))
(assert (= (x [1 2 3]) '(1 1 2 2 3 3)))
(assert (= (x [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
(assert (= (x [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (x [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

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
(defn cnt [coll]
  (apply + (map (fn [_] 1) coll)))
(assert (= (cnt '(1 2 3 3 1)) 5))
(assert (= (cnt "Hello World") 11))
(assert (= (cnt [[1 2] [3 4] [5 6]]) 3))
(assert (= (cnt '(13)) 1))
(assert (= (cnt '(:a :b :c)) 3))

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
(defn my-reverse [coll]
    (loop [current coll reversed []]
      (if (empty? current)
        reversed
        (recur (butlast current)
               (conj reversed (last current))))))
(assert (= (my-reverse [1 2 3 4 5]) [5 4 3 2 1]))
(assert (= (my-reverse (sorted-set 5 7 2 7)) '(7 5 2)))
(assert (= (my-reverse [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))

"Palindrome Detector"
"Write a function which returns true if the given sequence is a palindrome.
Hint: \"racecar\" does not equal '(\\r \\a \\c \\e \\c \\a \\r)"
(defn palindrome? [sqnce]
  (= (vec sqnce) (vec (reverse sqnce))))
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
  (clojure.string/join (filter (fn [c] (Character/isUpperCase c)) s)))
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
    (loop [current start coll []]
      (if (= current end)
        coll
        (recur (inc current) (conj coll current)))))
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
(defn my-interleave [coll1, coll2]
  (let [len (min (count coll1) (count coll2))]
    (reduce concat (map vector (take len coll1) (take len coll2)))))
(assert (= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
(assert (= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4)))
(assert (= (my-interleave [1 2 3 4] [5]) [1 5]))
(assert (= (my-interleave [30 20] [25 15]) [30 25 20 15]))

"Compress a Sequence"
"Write a function which removes consecutive duplicates from a sequence."
(defn compress [coll]
  (map second
       (filter (fn [[index item]] (not= item (get coll (dec index))))
               (map vector (range) coll))))
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
  (if (> n 1)
    (apply interleave (take n (repeat coll)))
    coll))
(assert (= (replicate [1 2 3] 2) '(1 1 2 2 3 3)))
(assert (= (replicate [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
(assert (= (replicate [4 5 6] 1) '(4 5 6)))
(assert (= (replicate [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (replicate [44 33] 2) [44 44 33 33]))

"Interpose a Seq"
"Write a function which separates the items of a sequence by an arbitrary value."
(defn my-interpose [x coll]
  (butlast (reduce concat
                   (map (fn [item] [item x]) coll))))
(assert (= (my-interpose 0 [1 2 3]) [1 0 2 0 3]))
(assert (= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three"))
(assert (= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))

"Pack a Sequence"
"Write a function which packs consecutive duplicates into sub-lists."
(defn pack [coll]
    (loop [left (rest coll) result [] current [(first coll)]]
      (if (empty? left)
        (if (empty? result) result (conj result current))
        (let [current-item (first left)]
          (let [consec? (= current-item (last current))]
            (if consec?
              (recur (rest left) result (conj current current-item))
              (recur (rest left) (conj result current) [current-item])))))))
(assert (= (pack [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
(assert (= (pack [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
(assert (= (pack [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))

"Drop Every Nth Item"
"Write a function which drops every Nth item from a sequence."
(defn drop-every-nth [coll n]
  (map second
       (filter (fn [[idx val]] (not (zero? (mod (inc idx) n))))
               (map vector (range) coll))))
(assert (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
(assert (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
(assert (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6]))

"Intro to Destructuring"
"Let bindings and function parameter lists support destructuring."
(= [2 4] (let [[a b c d e f g] (range)] [c e]))

"Split a sequence"
"Write a function which will split a sequence into two parts."
(defn my-split [n coll]
  [(subvec coll 0 n) (subvec coll n (count coll))])
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
    (loop [div (min x y)]
      (if (and (zero? (mod x div)) (zero? (mod y div)))
        div
        (recur (dec div)))))
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
  (map (fn [i] (reduce (fn [v f] (f v))
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
  (map (fn [[x]] (-> x str Integer/parseInt))
       (partition 1 (str (* a b)))))
(assert (= (prod-digits 1 1) [1]))
(assert (= (prod-digits 99 9) [8 9 1]))
(assert (= (prod-digits 999 99) [9 8 9 0 1]))

"Cartesian Product"
"Write a function which calculates the Cartesian product of two sets."
(defn cartesian [coll1 coll2]
  (set (for [item1 coll1 item2 coll2] [item1 item2])))
(= (cartesian #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
(= (cartesian #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
(= 300 (count (cartesian (into #{} (range 10))
                         (into #{} (range 30)))))

"Group a Sequence"
"Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s. The value at each key should be a vector of corresponding items in the order they appear in s."
(defn my-group-by [f coll]
  (loop [left coll result {}]
    (if (empty? left)
      result
      (let [k (f (first left)) v (first left)]
        (recur (rest left)
               (assoc result k (conj (get result k []) v)))))))
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
