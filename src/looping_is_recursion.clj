(ns looping-is-recursion)

(defn power [base exp]
  ":(")

(defn recur-factorial [number]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc n) (dec n))))]
    (helper 1 number)))

(defn power [base exp]
  (let [helper (fn [base exp curr]
                 (if (zero? exp)
                   curr
                   (recur base (dec exp) (* curr base))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [sequ curr]
                 (if (empty? sequ)
                   curr
                   (recur (rest sequ) (first sequ))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq-a seq-b curr]
    (cond
      (and (empty? seq-a) (empty? seq-b))
        true
     (or (empty? seq-a) (empty? seq-b))
        false
      (= (first seq-a) (first seq-b))
        (recur (rest seq-a) (rest seq-b) true)
     :else
        false))]
    (helper seq1 seq2 true)))

(defn find-first-index [pred a-seq]
  (loop [p pred
         sequ a-seq
         posi 0]
    (cond
      (empty? sequ)
        nil
      (p (first sequ))
        posi
      :else
        (recur p (rest sequ) (inc posi)))))

(defn avg [a-seq]
  (loop [sequ a-seq
         curr 0
         ele 0]
    (cond
      (empty? sequ)
        (/ curr ele)
      :else
        (recur (rest sequ) (+ curr (first sequ)) (inc ele)))))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [sequ a-seq
         m #{}]
    (cond
      (empty? sequ)
        m
      :else
        (recur (rest sequ) (toggle m (first sequ))))))

(defn fast-fibo [n]
  (loop [curr n
         f1 0
         f 1]
    (cond
      (= 0 curr)
        f1
      :else
        (recur (dec curr) f (+ f f1)))))

(defn cut-at-repetition [a-seq]
  (loop [sequ a-seq
         curr []]
    (cond
      (empty? sequ)
        curr
      (= (first curr) (first sequ))
        curr
      :else
        (recur (rest sequ) (conj curr (first sequ))))))
