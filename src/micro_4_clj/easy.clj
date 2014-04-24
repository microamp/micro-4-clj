"Last Element:"
"Write a function which returns the last element in a sequence."
(def x last)
(assert (= (x [1 2 3 4 5]) 5))
(assert (= (x '(5 4 3)) 3))
(assert (= (x ["b" "c" "d"]) "d"))

"Penultimate Element:"
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
(def my-reverse
  (fn [coll]
    (loop [current coll reversed []]
      (if (empty? current)
        reversed
        (recur (butlast current)
               (conj reversed (last current)))))))
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
(def my-range
  (fn [start end]
    (loop [current start coll []]
      (if (= current end)
        coll
        (recur (inc current) (conj coll current))))))
(= (my-range 1 4) '(1 2 3))
(= (my-range -2 2) '(-2 -1 0 1))
(= (my-range 5 8) '(5 6 7))

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
