"Nothing but the Truth"
"This is a clojure form. Enter a value which will make the form evaluate to true. Don't over think it! If you are confused, see the getting started page. Hint: true is equal to true."
(assert (= true true))

"Simple Math"
"If you are not familiar with polish notation, simple arithmetic might seem confusing. Note: Enter only enough to fill in the blank (in this case, a single number) - do not retype the whole problem."
(assert (= (- 10 (* 2 3)) 4))

"Intro to Strings"
"Clojure strings are Java strings. This means that you can use any of the Java string methods on Clojure strings."
(assert (= "HELLO WORLD" (.toUpperCase "hello world")))

"Intro to Lists"
"Lists can be constructed with either a function or a quoted form."
(assert (= (list :a :b :c) '(:a :b :c)))

"Lists: conj"
"When operating on a list, the conj function will return a new list with one or more items \"added\" to the front."
(def x '(1 2 3 4))
(assert (= x (conj '(2 3 4) 1)))
(assert (= x (conj '(3 4) 2 1)))

"Intro to Vectors"
"Vectors can be constructed several ways. You can compare them with lists."
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

"Vectors: conj"
"When operating on a Vector, the conj function will return a new vector with one or more items \"added\" to the end."
(def x [1 2 3 4])
(assert (= x (conj [1 2 3] 4)))
(assert (= x (conj [1 2] 3 4)))

"Intro to Sets"
"Sets are collections of unique values."
(def x #{:a :b :c :d})
(assert (= x (set '(:a :a :b :c :c :c :c :d :d))))
(assert (= x (clojure.set/union #{:a :b :c} #{:b :c :d})))

"Sets: conj"
"When operating on a set, the conj function returns a new set with one or more keys \"added\"."
(assert (= #{1 2 3 4} (conj #{1 4 3} 2)))

"Intro to Maps"
"Maps store key-value pairs. Both maps and keywords can be used as lookup functions. Commas can be used to make maps more readable, but they are not required."
(def x 20)
(assert (= x ((hash-map :a 10, :b 20, :c 30) :b)))
(assert (= x (:b {:a 10, :b 20, :c 30})))

"Maps: conj"
"When operating on a map, the conj function returns a new map with one or more key-value pairs \"added\"".
(assert (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))

"Intro to Sequences"
"All Clojure collections support sequencing. You can operate on sequences with functions like first, second, and last."
(def x 3)
(assert (= x (first '(3 2 1))))
(assert (= x (second [2 3 4])))
(assert (= x (last (list 1 2 3))))

"Sequences: rest"
"The rest function will return all the items of a sequence except the first."
(assert (= [20 30 40] (rest [10 20 30 40])))

"Intro to Functions"
"Clojure has many different ways to create functions."
(def x 8)
(assert (= x ((fn add-five [x] (+ x 5)) 3)))
(assert (= x ((fn [x] (+ x 5)) 3)))
(assert (= x (#(+ % 5) 3)))
(assert (= x ((partial + 5) 3)))

"Double Down"
"Write a function which doubles a number."
(def x #(* % 2))
(assert (= (x 2) 4))
(assert (= (x 3) 6))
(assert (= (x 11) 22))
(assert (= (x 7) 14))

"Hello World"
"Write a function which returns a personalized greeting."
(def x #(str "Hello, " % "!"))
(assert (= (x "Dave") "Hello, Dave!"))
(assert (= (x "Jenn") "Hello, Jenn!"))
(assert (= (x "Rhea") "Hello, Rhea!"))

"Sequences: map"
"The map function takes two arguments: a function (f) and a sequence (s). Map returns a new sequence consisting of the result of applying f to each item of s. Do not confuse the map function with the map data structure."
(assert (= '(6 7 8) (map #(+ % 5) '(1 2 3))))

"Sequences: filter"
"The filter function takes two arguments: a predicate function (f) and a sequence (s). Filter returns a new sequence consisting of all the items of s for which (f item) returns true."
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

"Local bindings"
"Clojure lets you give local names to values using the special let-form."
(def x 7)
(assert (= x (let [x 5] (+ 2 x))))
(assert (= x (let [x 3, y 10] (- y x))))
(assert (= x (let [x 21] (let [y 3] (/ x y)))))

"Rearranging Code: ->"
"The -> macro threads an expression x through a variable number of forms. First, x is inserted as the second item in the first form, making a list of it if it is not a list already. Then the first form is inserted as the second item in the second form, making a list of that form if necessary. This process continues for all the forms. Using -> can sometimes make your code more readable."
(assert (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
           (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
           5))

"Rearranging Code: ->>"
"The ->> macro threads an expression x through a variable number of forms. First, x is inserted as the last item in the first form, making a list of it if it is not a list already. Then the first form is inserted as the last item in the second form, making a list of that form if necessary. This process continues for all the forms. Using ->> can sometimes make your code more readable."
(assert (= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
           11))

"For the win"
"Clojure's for macro is a tremendously versatile mechanism for producing a sequence based on some other sequence (s). It can take some time to understand how to use it properly, but that investment will be paid back with clear, concise sequence-wrangling later. With that in mind, read over these for expressions and try to see how each of them produces the same result."
(def val [1 5 9 13 17 21 25 29 33 37])
(assert (= val (for [x (range 40)
                     :when (= 1 (rem x 4))]
                 x)))
(assert (= val (for [x (iterate #(+ 4 %) 0)
                     :let [z (inc x)]
                     :while (< z 40)]
                 z)))
(assert (= val (for [[x y] (partition 2 (range 20))]
                 (+ x y))))

"Logical falsity and truth"
"In Clojure, only nil and false represent the values of logical falsity in conditional tests - anything else is logical truth."
(def x 1)
(= x (if-not false 1 0))
(= x (if-not nil 1 0))
(= x (if true 1 0))
(= x (if [] 1 0))
(= x (if [0] 1 0))
(= x (if 0 1 0))
(= x (if 1 1 0))

"Map Defaults"
"When retrieving values from a map, you can specify default values in case the key is not found:"
"(= 2 (:foo {:bar 0, :baz 1} 2))"
"However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map."
(def x
  (fn [val keys]
    (reduce merge
            (map (fn [k] {k val}) keys))))
(assert (= (x 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
(assert (= (x "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
(assert (= (x [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))


"Let it Be"
"Can you bind x, y, and z so that these are all true?"
(assert (= 10 (let [z 1 y 3 x 7] (+ x y))))
(assert (= 4 (let [z 1 y 3 x 7] (+ y z))))
(assert (= 1 (let [z 1 y 3 x 7] z)))

"Intro to Reduce"
"Reduce takes a 2 argument function and an optional starting value. It then applies the function to the first 2 items in the sequence (or the starting value and the first element of the sequence). In the next iteration the function will be called on the previous return value and the next item from the sequence, thus reducing the entire collection to one value. Don't worry, it's not as complicated as it sounds."
(= 15 (reduce + [1 2 3 4 5]))
(=  0 (reduce + []))
(=  6 (reduce + 1 [2 3]))

"A nil key"
"Write a function which, given a key and map, returns true if the map contains an entry with that key and its value is nil."
(defn x [key map]
  (and (contains? map key)
       (nil? (map key))))
(assert (true?  (x :a {:a nil :b 2})))
(assert (false? (x :b {:a nil :b 2})))
(assert (false? (x :c {:a nil :b 2})))

"Subset and Superset"
"Set A is a subset of set B, or equivalently B is a superset of A, if A is \"contained\" inside B. A and B may coincide."
(def x #{1 2})
(clojure.set/superset? x #{2})
(clojure.set/subset? #{1} x)
(clojure.set/superset? x #{1 2})
(clojure.set/subset? #{1 2} x)

"Recurring Theme"
"Clojure only has one non-stack-consuming looping construct: recur. Either a function or a loop can be used as the recursion point. Either way, recur rebinds the bindings of the recursion point to the values it is passed. Recur must be called from the tail-position, and calling it elsewhere will result in an error."
(def x [7 6 5 4 3])
(assert (= x
           (loop [x 5
                  result []]
             (if (> x 0)
               (recur (dec x) (conj result (+ 2 x)))
               result))))

"Simple Recursion"
"A recursive function is a function which calls itself. This is one of the fundamental techniques used in functional programming."
(def x [5 4 3 2 1])
(assert (= x ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))

"Regular Expressions"
"Regex patterns are supported with a special reader macro."
(def x "ABC")
(assert (= x (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))
