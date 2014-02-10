(ns looping-is-recursion)

(defn power [base exp]
  (let [rec (fn [acc exp]
              (if (zero? exp)
                acc
                (recur (* acc base) (dec exp))
                ))]
    (rec 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq)
    nil
    (empty? (rest a-seq))
    (first a-seq)
    :else
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (not= (count seq1) (count seq2))
    false
    (empty? seq1)
    true
    (= (first seq1) (first seq2))
    (recur (rest seq1) (rest seq2))
    :else
    false
    ))

(defn find-first-index [pred a-seq]
  (loop [index 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else
      (recur (inc index) (rest a-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         s a-seq]
    (cond
      (empty? a-seq)
      0
      (empty? s)
      (/ acc (count a-seq))
      :else
      (recur (+ acc (first s)) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set k] (if (a-set k)
                               (disj a-set k)
                               (conj a-set k)))]
    (loop [acc #{}
           r a-seq]
      (if (empty? r)
        acc
        (recur (toggle acc (first r)) (rest r))))))

(defn fast-fibo [n]
  (loop [i 0
         f-2 0
         acc 0]
    (cond
      (= i n) acc
      (= i 0) (recur (inc i) 0 1)
      (= i 1) (recur (inc i) 1 1)
      :else
      (recur (inc i) acc (+ acc f-2)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         a-set #{}
         a-seq a-seq]
    (cond
      (empty? a-seq) acc
      (a-set (first a-seq)) acc
      :else
      (recur (conj acc (first a-seq))
             (conj a-set (first a-seq))
             (rest a-seq)))))

