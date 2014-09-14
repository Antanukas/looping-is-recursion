(ns looping-is-recursion)

(defn power [base exp]
  (let [helper
        (fn [acc base exp]
          (if (= 0 exp)
            acc
            (recur (* acc base) base (- exp 1))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (or (empty? seq1) (empty? seq2))
      false
    (not (= (first seq1) (first seq2)))
      false
    :else
      (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq)
        nil
      (pred (first seq))
        index
      :else
        (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [count 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum (if (zero? count) 1 count))
      (recur (inc count) (+ sum (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-elemets #{}
         seq a-seq]
    (if (empty? seq)
      odd-elemets
      (recur
        (toggle odd-elemets (first seq))
        (rest seq)))))

(defn fast-fibo [n]
  (loop [fn-1 1
         fn-2 1
         en n]
    (cond
      (= en 0)
        0
      (= en 1)
        fn-1
      (= en 2)
        fn-2
      :else
        (recur fn-2 (+ fn-1 fn-2) (dec en)))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         repetitions #{}
         seq a-seq
         prev nil]
    (let [elem (first seq)]
      (if (or
            (empty? seq)
            (= prev elem)
            (contains? repetitions elem))
        res
        (recur
          (conj res elem)
          (conj repetitions elem)
          (rest seq)
          elem)))))