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
