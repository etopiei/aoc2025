(require '[clojure.string :as str])

;; (def input (slurp "input.txt"))
(def input (slurp "test.txt"))

(defn remove-last [items]
  (reverse (rest (reverse items))))

(defn find-highest [items]
  (first (sort #(compare %2 %) items)))

(defn find-first-digit [bank]
  (find-highest (remove-last bank)))

(defn bank-from-line [line]
  (map #(parse-long (str %)) line))

(def banks (map bank-from-line (str/split input #"\n")))

(def first-digits (map find-first-digit banks))

(defn find-second-digit [first-digit bank]
  (let [remaining (rest (drop-while #(not= first-digit %) bank))] 
    (find-highest remaining)))

(def second-digits (map #(find-second-digit (first %) (second %)) (map vector first-digits banks)))

 (defn part1 []
   (reduce + (map #(parse-long (str (first %) (second %))) (map vector first-digits second-digits))))

(println (part1))

(defn drop-n-from-end [items n]
  (reverse (drop n (reverse items))))

(defn get-highest-digit [bank n]
  (find-highest (drop-n-from-end bank (dec n))))

(defn remainder [bank target]
  "Find the first instance of target and return the remaining items"
  (rest (drop-while #(not= target %) bank)))

(defn largest-number-digits-with-n-digits [bank n current-digits]
  (if (= n 0) current-digits
      (let [new-digit (get-highest-digit bank n)] 
        (largest-number-digits-with-n-digits (remainder bank new-digit) (dec n) (conj current-digits new-digit)))))

(defn largest-number-with-n-digits [bank n]
  (parse-long (apply str (largest-number-digits-with-n-digits bank n []))))

(defn part2 []
  (reduce + (map #(largest-number-with-n-digits % 12) banks)))

(println (part2))
