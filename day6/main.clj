(require '[clojure.string :as str])

;;(def input (slurp "test.txt"))
(def input (slurp "input.txt"))

(defn get-op [x] (get {"*" * "+" +} x))

(def lines (str/split input #"\n"))
(def ops (map get-op (str/split (last lines) #"\s+")))
(def values (map #(map parse-long (remove empty? (str/split % #"\s+"))) (rest (reverse lines))))
(def columns (apply mapv vector values)) ; Thanks to Clojuredocs for this neat trick!

(defn part1 []
  (let [totals (map apply ops columns)]
    (reduce + totals)))

(println (part1))

(def op-indicies (keep-indexed #(if (str/blank? (str %2)) nil %) (last lines)))
(def char-ranges (partition 2 1 (concat op-indicies [(count (first lines))])))

(defn remove-last [coll]
  (reverse (rest (reverse coll))))

(defn remove-space [coll]
  (filter #(not= \space %) coll))

(defn make-number [ind]
  (parse-long (apply str (remove-space (map #(get % ind) (remove-last lines))))))

(defn make-numbers-for-char-range [[beg end]]
  (let [stop (dec end)]
    (filter #(not (nil? %)) (map make-number (range beg end)))))

(def p2values (map make-numbers-for-char-range char-ranges))

(defn part2 []
  (reduce + (map apply ops p2values)))

(println (part2))
