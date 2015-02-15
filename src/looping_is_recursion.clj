(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc rest-aseq]
                 (if (empty? rest-aseq)
                   acc
                   (recur (first rest-aseq) (rest rest-aseq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else
      (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [rest-aseq a-seq
         index 0]
    (cond
      (empty? rest-aseq) nil
      (pred (first rest-aseq)) index
      :else
        (recur (rest rest-aseq) (inc index)))))

(defn avg [a-seq]
  (loop [rest-aseq a-seq
         total 0]
    (if (empty? rest-aseq)
      (/ total (count a-seq))
      (recur (rest rest-aseq) (+ total (first rest-aseq))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odds #{}
           rest-aseq a-seq]
      (if (empty? rest-aseq)
        odds
        (recur (toggle odds (first rest-aseq)) (rest rest-aseq))))))

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

