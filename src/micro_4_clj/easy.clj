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
