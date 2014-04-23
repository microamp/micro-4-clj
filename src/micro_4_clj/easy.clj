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
(defn except-last [coll]
  (take (- (count coll) 1) coll))
(def my-reverse
  (fn [coll]
    (loop [current coll reversed []]
      (if (empty? current)
        reversed
        (recur (except-last current)
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
