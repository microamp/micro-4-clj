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

"Duplicate a Sequence"
"Write a function which duplicates each element of a sequence."
(def x #(interleave % %))
(assert (= (x [1 2 3]) '(1 1 2 2 3 3)))
(assert (= (x [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
(assert (= (x [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (x [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
