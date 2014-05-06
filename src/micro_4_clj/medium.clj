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
(= 3 ((flipper nth) 2 [1 2 3 4 5]))
(= true ((flipper >) 7 8))
(= 4 ((flipper quot) 2 8))
(= [1 2 3] ((flipper take) [1 2 3 4 5] 3))

"Longest Increasing Sub-Seq"
"Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify."
(defn liss [coll]
  (loop [s (rest coll)
         consecs [[(first coll)]]]
    (if (empty? s)
      (let [longest (reduce (fn [c1 c2] (if (>= (count c1) (count c2)) c1 c2))
                            consecs)]
        (if (>= (count longest) 2) longest []))
      (let [current-item (first s)
            last-item (-> consecs last last)]
        (recur (rest s) (if (> current-item last-item)
                          (conj (vec (butlast consecs))
                                (conj (last consecs) current-item))
                          (conj consecs
                                [current-item])))))))
(assert (= (liss [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(assert (= (liss [5 6 1 3 2 7]) [5 6]))
(assert (= (liss [2 3 3 4 5]) [3 4 5]))
(assert (= (liss [7 6 5 4]) []))
