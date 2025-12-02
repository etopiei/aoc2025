(require '[clojure.string :as str])

;;(def input (slurp "test.txt"))
(def input (slurp "input.txt"))

(defn range-from-string [input]
  (let [start-end (map #(parse-long (str/trim %)) (str/split input #"-"))]
    (into [] (range (first start-end) (inc (second start-end))))))

(def all-ids (mapcat range-from-string (str/split input #",")))

(defn id-check [id]
  (let [id-str (str id)]
    (=
     (subs id-str 0 (/ (count id-str) 2))
     (subs id-str (/ (count id-str) 2)))))

(defn part1 []
  (reduce + (filter id-check all-ids)))

(println (part1))

(defn with-repeat-check [test sub-length]
  (= test (apply str (repeat (/ (count test) sub-length) (subs test 0 sub-length)))))

(defn id-check2 [id]
  (let [id-str (str id)
        strlen (count id-str)]
    (if (= 1 strlen)
      false 
      (some true? (map #(with-repeat-check id-str %) (range 1 (inc (/ strlen 2))))))))

(defn part2 []
  (reduce + (filter id-check2 all-ids)))

(println (part2))

