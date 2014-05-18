"Analyze a Tic-Tac-Toe Board"
"A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won."
(defn ttt [matrix]
  (let [[[r1c1 r1c2 r1c3]
         [r2c1 r2c2 r2c3]
         [r3c1 r3c2 r3c3]] matrix]
    (first
     (first
      (filter #(= (count %) 1)
              (filter #(not (contains? % :e))
                      (map set [[r1c1 r1c2 r1c3]
                                [r2c1 r2c2 r2c3]
                                [r3c1 r3c2 r3c3]
                                [r1c1 r2c1 r3c1]
                                [r1c2 r2c2 r3c2]
                                [r1c3 r2c3 r3c3]
                                [r1c1 r2c2 r3c3]
                                [r1c3 r2c2 r3c1]])))))))
(assert (= nil (ttt [[:e :e :e]
                     [:e :e :e]
                     [:e :e :e]])))
(assert (= :x (ttt [[:x :e :o]
                    [:x :e :e]
                    [:x :e :o]])))
(assert (= :o (ttt [[:e :x :e]
                    [:o :o :o]
                    [:x :e :x]])))
(assert (= nil (ttt [[:x :e :o]
                     [:x :x :e]
                     [:o :x :o]])))
(assert (= :x (ttt [[:x :e :e]
                    [:o :x :e]
                    [:o :e :x]])))
(assert (= :o (ttt [[:x :e :o]
                    [:x :o :e]
                    [:o :e :x]])))
(assert (= nil (ttt [[:x :o :x]
                     [:x :o :x]
                     [:o :x :o]])))
