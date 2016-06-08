(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond (= 1 n)
                          acc
                       (zero? n)
                         1
                       :else
                         (recur (* acc base) (dec n))
                       )
                 )]
    (helper base exp)
    )
  )

(defn last-element [a-seq]
  (let [helper (fn [b-seq]
                 (cond (empty? b-seq)
                         nil
                       (= 1 (count b-seq))
                         (first b-seq)
                       :else
                         (recur (rest b-seq))
                       )
                 )]
    (helper a-seq)
    )
  )

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond (not (= (count a-seq) (count b-seq)))
                         false
                       (not (= (first a-seq) (first b-seq)))
                         false
                       (empty? a-seq)
                         true
                       :else
                         (recur (rest a-seq) (rest b-seq))
                       )
                 )]
    (helper seq1 seq2)
    )
  )

(defn find-first-index [pred a-seq]
  (loop [n 0
         b-seq a-seq]
    (cond (empty? b-seq)
            nil
          (pred (first b-seq))
            n
          :else
            (recur (inc n)(rest b-seq))
          )
    )
  )

(defn avg [a-seq]
  (loop [n 0
         nbr 0
         b-seq a-seq]
    (cond (empty? b-seq)
            (/ n nbr)
          :else
            (recur (+ n (first b-seq)) (inc nbr) (rest b-seq))
          )
    )
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (loop [orig-par (set a-seq)
         parities (set a-seq)
         b-seq a-seq]
       (cond (empty? b-seq)
               (clojure.set/difference orig-par parities)
             :else
               (recur orig-par (toggle parities (first b-seq)) (rest b-seq))
             )
    )
  )

(defn fast-fibo [n]
  (loop [nbr 1
         fnbr 1
         fn-1 0]
    (cond (zero? n)
            0
          (= n nbr)
            fnbr
          :else
            (recur (inc nbr) (+ fnbr fn-1) fnbr)
          )
    )
  )

(defn cut-at-repetition [a-seq]
  (loop [return []
         b-seq a-seq]
    (let [cont? (fn [c-seq elem]
                  (= elem (first c-seq))
                  )]
      (cond (empty? b-seq)
              return
            (cont? return (first b-seq))
              return
            :else
              (recur (conj return (first b-seq)) (rest b-seq))
            )
      )
    )
  )
