"Rotate Sequence"
"Write a function which can rotate a sequence in either direction."
(defn rotate [n c]
  (if (pos? n)
    (take (count c) (drop n (cycle c)))
    (reverse (take (count c) (drop (Math/abs n) (cycle (reverse c)))))))
(assert (= (rotate 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(assert (= (rotate -2 [1 2 3 4 5]) '(4 5 1 2 3)))
(assert (= (rotate 6 [1 2 3 4 5]) '(2 3 4 5 1)))
(assert (= (rotate 1 '(:a :b :c)) '(:b :c :a)))
(assert (= (rotate -4 '(:a :b :c)) '(:c :a :b)))

"Reverse Interleave"
"Write a function which reverses the interleave process into x number of subsequences."
(defn reverse-interleave [coll i]
  (apply map vector (partition i coll)))
(assert (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
(assert (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
(assert (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))

"Split by Type"
"Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases)."
(defn split-by-type [coll]
  (vals (group-by type coll)))
(assert (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
(assert (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
(assert (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))

"Count Occurrences"
"Write a function which returns a map containing the number of occurences of each distinct item in a sequence."
(defn my-frequencies [coll]
  (into {} (map (fn [[k v]] [k (count v)])
                (group-by identity coll))))
(assert (= (my-frequencies [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
(assert (= (my-frequencies [:b :a :b :a :b]) {:a 2, :b 3}))
(assert (= (my-frequencies '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))

"Find Distinct Items"
"Write a function which removes the duplicates from a sequence. Order of the items must be maintained."
(defn my-distinct [coll]
  (sort-by #(.indexOf coll %) (keys (group-by identity coll))))
(assert (= (my-distinct [1 2 1 3 1 2 4]) [1 2 3 4]))
(assert (= (my-distinct [:a :a :b :b :c :c]) [:a :b :c]))
(assert (= (my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
(assert (= (my-distinct (range 50)) (range 50)))

"Partition a Sequence"
"Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned."
(defn my-partition [n coll]
  (loop [left coll result []]
    (let [temp (take n left)]
      (if (< (count temp) n)
        result
        (recur (drop n left) (conj result temp))))))
(assert (= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
(assert (= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
(assert (= (my-partition 3 (range 8)) '((0 1 2) (3 4 5))))

"Sequence Reductions"
"Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy."
(defn my-reductions
  ([func coll]
     (map (fn [n] (reduce func (take n coll))) (rest (range))))
  ([func first-item coll]
     (let [c (cons first-item coll)]
       (take (count c)
             (map (fn [n] (reduce func (take n c))) (rest (range)))))))
(assert (= (take 5 (my-reductions + (range))) [0 1 3 6 10]))
(assert (= (my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
(assert (= (last (my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))

"intoCamelCase"
"When working with java, you often need to create an object with fieldsLikeThis, but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings."
(defn camel-case [s]
  (let [splitted (clojure.string/split s #"\-")]
    (if (= (count splitted) 1)
      (first splitted)
      (str (first splitted)
           (apply str (map #(reduce (fn [a b] (str a b))
                                    (-> (first %) str .toUpperCase)
                                    (rest %))
                           (rest splitted)))))))
(assert (= (camel-case "something") "something"))
(assert (= (camel-case "multi-word-key") "multiWordKey"))
(assert (= (camel-case "leaveMeAlone") "leaveMeAlone"))

"Sequs Horribilis"
"Create a function which takes an integer and a nested collection of integers as arguments. Analyze the elements of the input collection and return a sequence which maintains the nested structure, and which includes all elements starting from the head whose sum is less than or equal to the input integer."
(defn sequs-h
  ([limit coll]
     (sequs-h limit coll [] 0))
  ([limit coll result sum]
     (if (empty? coll)
       result
       (let [first-item (first coll)]
         (if (integer? first-item)
           (let [after-sum (+ sum first-item)]
             (if (> after-sum limit)
               result
               (sequs-h (- limit first-item)
                        (rest coll)
                        (conj result first-item)
                        (+ sum first-item))))
           (conj result
                 (sequs-h limit
                          first-item)))))))
(assert (=  (sequs-h 10 [1 2 [3 [4 5] 6] 7])
            '(1 2 (3 (4)))))
(assert (=  (sequs-h 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
            '(1 2 (3 (4 (5 (6 (7))))))))
(assert (=  (sequs-h 9 (range))
            '(0 1 2 3)))
(assert (=  (sequs-h 1 [[[[[1]]]]])
            '(((((1)))))))
(assert (=  (sequs-h 0 [1 2 [3 [4 5] 6] 7])
            '()))
(assert (=  (sequs-h 0 [0 0 [0 [0]]])
            '(0 0 (0 (0)))))
(assert (=  (sequs-h 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
            '(-10 (1 (2 3 (4))))))

"Flipping out"
"Write a higher-order function which flips the order of the arguments of an input function."
(defn flipper [func]
  (fn [a b]
    (func b a)))
(assert (= 3 ((flipper nth) 2 [1 2 3 4 5])))
(assert (= true ((flipper >) 7 8)))
(assert (= 4 ((flipper quot) 2 8)))
(assert (= [1 2 3] ((flipper take) [1 2 3 4 5] 3)))

"Longest Increasing Sub-Seq"
"Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify."
(defn liss [coll]
  (loop [s (rest coll) longest [[]] current [(first coll)]]
    (if (empty? s)
      (let [result
            (if (>= (count longest) (count current)) longest current)]
        (if (>= (count result) 2) result []))
      (let [item (first s) prev (last current)]
        (if (> item prev)
          (recur (rest s) longest (conj current item))
          (recur (rest s)
                 (if (>= (count longest) (count current)) longest current)
                 [item]))))))
(assert (= (liss [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(assert (= (liss [5 6 1 3 2 7]) [5 6]))
(assert (= (liss [2 3 3 4 5]) [3 4 5]))
(assert (= (liss [7 6 5 4]) []))

"Function Composition"
"Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left."
(defn my-comp [& funcs]
  (fn [& args]
    (reduce (fn [r f] (f r))
            (apply (last funcs) args)
            (reverse (butlast funcs)))))
(assert (= [3 2 1] ((my-comp rest reverse) [1 2 3 4])))
(assert (= 5 ((my-comp (partial + 3) second) [1 2 3 4])))
(assert (= true ((my-comp zero? #(mod % 8) +) 3 5 7 9)))
(assert (= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")))

"Juxtaposition"
"Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list."
(defn my-juxt [& funcs]
  (fn [& args]
    (map #(apply % args) funcs)))
(assert (= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4)))
(assert (= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello")))
(assert (= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

"Word Sorting"
"Write a function that splits a sentence up into a sorted list of words. Capitalization should not affect sort order and punctuation should be ignored."
(defn word-sorting [sentence]
  (sort-by #(.toLowerCase %) (re-seq #"\w+" sentence)))
(assert (= (word-sorting  "Have a nice day.")
           ["a" "day" "Have" "nice"]))
(assert (= (word-sorting  "Clojure is a fun language!")
           ["a" "Clojure" "fun" "is" "language"]))
(assert (= (word-sorting  "Fools fall for foolish follies.")
           ["fall" "follies" "foolish" "Fools" "for"]))

"Merge with a Function"
"Write a function which takes a function f and a variable number of maps. Your function should return a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping (s) from the latter (left-to-right) should be combined with the mapping in the result by calling (f val-in-result val-in-latter)"
(defn my-merge-with [f & hmaps]
  (into {} (map (fn [[k v]] [k (reduce f v)])
                (map (fn [[k vecs]] [k (map (fn [v] (get v 1)) vecs)])
                     (group-by (fn [[k v]] k) (apply concat hmaps))))))
(assert (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
           {:a 4, :b 6, :c 20}))
(assert (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
           {1 7, 2 10, 3 15}))
(assert (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
           {:a [3 4 5], :b [6 7], :c [8 9]}))

"Oscilrate"
"Write an oscillating iterate: a function that takes an initial value and a variable number of functions. It should return a lazy sequence of the functions applied to the value in order, restarting from the first function after it hits the end."
(defn oscilrate [v & funcs]
  (reductions (fn [v f] (f v))
              v
              (cycle funcs)))
(assert (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
(assert (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
(assert (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))

"Insert between two items"
"Write a function that takes a two-argument predicate, a value, and a collection; and returns a new collection where the value is inserted between every two items that satisfy the predicate."
(defn ibti [p1 p2 coll]
  (filter identity
          (interleave coll
                      (concat (map (fn [[a b]] (if (p1 a b) p2))
                                   (partition 2 1 coll))
                              (repeat nil)))))
(assert (= '(1 :less 6 :less 7 4 3) (ibti < :less [1 6 7 4 3])))
(assert (= '(2) (ibti > :more [2])))
(assert (= [0 1 :x 2 :x 3 :x 4]  (ibti #(and (pos? %) (< % %2)) :x (range 5))))
(assert (empty? (ibti > :more ())))
(assert (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
           (take 12 (->> [0 1]
                         (iterate (fn [[a b]] [b (+ a b)]))
                         (map first)         ; fibonacci numbers
                         (ibti (fn [a b]      ; both even or both odd
                                 (= (mod a 2) (mod b 2)))
                               :same)))))

"Anagram Finder"
"Write a function which finds all the anagrams in a vector of words. A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. Each sub-set should have at least two words. Words without any anagrams should not be included in the result."
(defn anagram [coll]
  (set (map set
            (filter #(> (count %) 1)
                    (vals (group-by set coll))))))
(assert (= (anagram ["meat" "mat" "team" "mate" "eat"])
           #{#{"meat" "team" "mate"}}))
(assert (= (anagram ["veer" "lake" "item" "kale" "mite" "ever"])
           #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))

"Filter Perfect Squares"
"Given a string of comma separated integers, write a function which returns a new comma separated string that only contains the numbers which are perfect squares."
(defn fps [s]
  (apply str
   (interpose \,
              (filter (fn [n] (let [sqrt (Math/sqrt (Integer/parseInt n))]
                               (== sqrt (int sqrt))))
                      (clojure.string/split s #"\,")))))
(assert (= (fps "4,5,6,7,8,9") "4,9"))
(assert (= (fps "15,16,25,36,37") "16,25,36"))

"Happy numbers"
"Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not."
(defn square [n]
  (* n n))
(defn sum-of-squares [n]
  (reduce
   +
   (map (fn [x] (square (-> x str Integer/parseInt)))
        (str n))))
(defn happy-number? [n]
  (loop [x n nums []]
    (let [sum (sum-of-squares x)]
      (if (= sum 1)
        true
        (if (some #(= % sum) nums)
          false
          (recur sum (conj nums sum)))))))
(assert (= (happy-number? 7) true))
(assert (= (happy-number? 986543210) true))
(assert (= (happy-number? 2) false))
(assert (= (happy-number? 3) false))

"Identify keys and values"
"Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence."
(defn identify-kv [coll]
  (let [pairs (partition 2 (partition-by keyword? coll))]
    (if (empty? pairs)
      {}
      (apply merge
             (map (fn [[k v]]
                    (let [last-pairs [(last k) v]
                          butlast-pairs (for [k (butlast k)] [k []])]
                      (into {} (conj butlast-pairs last-pairs))))
                  pairs)))))
(assert (= {} (identify-kv [])))
(assert (= {:a [1]} (identify-kv [:a 1])))
(assert (= {:a [1], :b [2]} (identify-kv [:a 1, :b 2])))
(assert (= {:a [1 2 3], :b [], :c [4]} (identify-kv [:a 1 2 3 :b :c 4])))

"Intervals"
"Write a function that takes a sequence of integers and returns a sequence of \"intervals\". Each interval is a a vector of two integers, start and end, such that all integers between start and end (inclusive) are contained in the input sequence."
(defn intervals [c]
  (if (empty? c)
    []
    (map (fn [v] [(first v) (last v)])
         (let [sorted (-> c set sort)]
           (reduce
            (fn [v n]
              (let [but-last (butlast v)
                    last-v (last v)
                    last-n (-> v last last)]
                (if (= (inc last-n) n)
                  (if (empty? but-last)
                    [(conj last-v n)]
                    (conj (vec but-last) (conj last-v n)))
                  (conj v [n]))))
            [[(first sorted)]]
            (rest sorted))))))
(assert (= (intervals [1 2 3]) [[1 3]]))
(assert (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))
(assert (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))
(assert (= (intervals []) []))
(assert (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
           [[1 4] [6 6] [9 11] [13 17] [19 19]]))

"Balancing Brackets"
"When parsing a snippet of code it's often a good idea to do a sanity check to see if all the brackets match up. Write a function that takes in a string and returns truthy if all square [] round () and curly {} brackets are properly paired and legally nested, or returns falsey otherwise."
(defn balanced? [s]
  (let [brackets (re-seq #"[\[\]\(\)\{\}]" s)
        matching {"]" "[" ")" "(" "}" "{"}]
    (empty?
     (reduce
      (fn [o item]
        (let [closing? (contains? matching item)]
          (if (and closing? (= (last o) (get matching item)))
            (vec (butlast o))
            (conj o item))))
      []
      brackets))))
(assert (balanced? "This string has no brackets."))
(assert (balanced? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))
(assert (not (balanced? "(start, end]")))
(assert (not (balanced? "())")))
(assert (not (balanced? "[ { ] } ")))
(assert (balanced? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
(assert (not (balanced? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
(assert (not (balanced? "[")))

"Perfect Numbers"
"A number is \"perfect\" if the sum of its divisors equal the number itself. 6 is a perfect number because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise."
(defn perfect? [n]
  (let [divisors (filter #(= (mod n %) 0)
                         (range 1 (-> n Math/sqrt int inc)))]
    (= n (apply + (butlast (sort (mapcat #(vector % (/ n %))
                                         divisors)))))))
(assert (= (perfect? 6) true))
(assert (= (perfect? 7) false))
(assert (= (perfect? 496) true))
(assert (= (perfect? 500) false))
(assert (= (perfect? 8128) true))

"Prime Numbers"
"Write a function which returns the first x number of prime numbers."
(defn primes [n]
  (last (take n (iterate
                 (fn [dividers]
                   (let [d (first (filter (fn [x] (not-any? #(zero? (mod x %))
                                                           dividers))
                                          (drop (last dividers) (range))))]
                     (conj dividers d)))
                 [2]))))
(assert (= (primes 2) [2 3]))
(assert (= (primes 5) [2 3 5 7 11]))
(assert (= (last (primes 100)) 541))

"Black Box Testing"
"Clojure has many sequence types, which act in subtly different ways. The core functions typically convert them into a uniform \"sequence\" type and work with them that way, but it can be important to understand the behavioral and performance differences so that you know which kind is appropriate for your application.

Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.

Special Restrictions
class
type
Class
vector?
sequential?
list?
seq?
map?
set?
instance?
getClass"
(defn black-box [c]
  (let [poked (conj (empty c) [:a 1] [:a 1] [:b 2])]
    (if (= (count poked) 2)
      (if (= (:b poked) 2) :map :set)
      (if (= (first poked) '(:b 2)) :list :vector))))
(assert (= :map (black-box {:a 1, :b 2})))
(assert (= :list (black-box (range (rand-int 20)))))
(assert (= :vector (black-box [1 2 3 4 5 6])))
(assert (= :set (black-box #{10 (rand-int 5)})))
(assert (= [:map :set :vector :list] (map black-box [{} #{} [] ()])))

"Equivalence Classes"
"A function f defined on a domain D induces an equivalence relation on D, as follows: a is equivalent to b with respect to f if and only if (f a) is equal to (f b). Write a function with arguments f and D that computes the equivalence classes of D with respect to f."
(defn equivalence [f s]
  (set (map set (vals (group-by f s)))))
(assert (= (equivalence #(* % %) #{-2 -1 0 1 2})
           #{#{0} #{1 -1} #{2 -2}}))
(assert (= (equivalence #(rem % 3) #{0 1 2 3 4 5})
           #{#{0 3} #{1 4} #{2 5}}))
(assert (= (equivalence identity #{0 1 2 3 4})
           #{#{0} #{1} #{2} #{3} #{4}}))
(assert (= (equivalence (constantly true) #{0 1 2 3 4})
           #{#{0 1 2 3 4}}))

"Global take-while"
"take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time. What if you need to keep track of some state as you go over the sequence?

Write a function which accepts an integer n, a predicate p, and a sequence. It should return a lazy sequence of  items in the list up to, but not including, the nth item that satisfies the predicate."
(defn gtw [n p coll]
  (take (count (take-while
                #(> n (count (filter p (take % coll))))
                (iterate inc 1)))
        coll))
(assert (= [2 3 5 7 11 13]
           (gtw 4 #(= 2 (mod % 3))
                [2 3 5 7 11 13 17 19 23])))
(assert (= ["this" "is" "a" "sentence"]
           (gtw 3 #(some #{\i} %)
                ["this" "is" "a" "sentence" "i" "wrote"])))
(assert (= ["this" "is"]
           (gtw 1 #{"a"}
                ["this" "is" "a" "sentence" "i" "wrote"])))

"Sequence of pronunciations"
"Write a function that returns a lazy sequence of \"pronunciations\" of a sequence of numbers. A pronunciation of each element in the sequence consists of the number of repeating identical numbers and the number itself. For example, [1 1] is pronounced as [2 1] (\"two ones\"), which in turn is pronounced as [1 2 1 1] (\"one two, one one\").

Your function should accept an initial sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being a pronunciation of the previous element."
(defn pronunciations [coll]
  (let [pack (fn [c]
               (let [diffs (filter #(not (= (get c %)
                                            (get c (dec %))))
                                   (range 1 (count c)))]
                 (map (fn [[start end]] (subvec c start end))
                      (partition 2 1 (concat [0] diffs [(count c)])))))]
    (rest (iterate #(mapcat (fn [packed] [(count packed)
                                         (first packed)])
                            (pack (vec %)))
                   coll))))
(assert (= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1]))))
(assert (= [3 1 2 4] (first (pronunciations [1 1 1 4 4]))))
(assert (= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6)))
(assert (= 338 (count (nth (pronunciations [3 2]) 15))))

"Decurry"
"Write a function that accepts a curried function of unknown arity n. Return an equivalent function of n arguments.

You may wish to read this."
(defn decurry [curried]
  (fn [& values]
    (reduce (fn [f v] (f v))
            (partial curried)
            values)))
(assert (= 10 ((decurry (fn [a]
                          (fn [b]
                            (fn [c]
                              (fn [d]
                                (+ a b c d))))))
               1 2 3 4)))
(assert (= 24 ((decurry (fn [a]
                          (fn [b]
                            (fn [c]
                              (fn [d]
                                (* a b c d))))))
               1 2 3 4)))
(assert (= 25 ((decurry (fn [a]
                          (fn [b]
                            (* a b))))
               5 5)))

"Euler's Totient Function"
"Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function f (x) is defined as the number of positive integers less than x which are coprime to x. The special case f (1) equals 1. Write a function which calculates Euler's totient function."
(defn gcd [x y]
  (let [[lower higher] (if (> x y) [y x] [x y])]
    (first (filter #(zero? (mod higher %))
                   (filter #(zero? (mod lower %))
                           (iterate dec lower))))))
(defn totient [x]
  (if (= x 1)
    1
    (count (filter #(= % 1)
                   (map #(gcd % x)
                        (range 1 x))))))
(assert (= (totient 1) 1))
(assert (= (totient 10) (count '(1 3 7 9)) 4))
(assert (= (totient 40) 16))
(assert (= (totient 99) 60))

"Lazy Searching"
"Given any number of sequences, each sorted from smallest to largest, find the smallest single number which appears in all of the sequences. The sequences may be infinite, so be careful to search lazily."
(defn lazy-search [& seqs]
  (let [first-items (map first seqs)]
    (if (apply = first-items)
      (first first-items)
      (let [smallest (apply min first-items)]
        (apply lazy-search (map (fn [s] (if (= (first s) smallest) (rest s) s))
                                seqs))))))
(assert (= 3 (lazy-search [3 4 5])))
(assert (= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
(assert (= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13])))
(assert (= 64 (lazy-search (map #(* % % %) (range)) ;; perfect cubes
                           (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                           (iterate inc 20)))) ;; at least as large as 20
