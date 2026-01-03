(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (slurp "input.txt"))
;;(def input (slurp "test.txt"))

(def rows (str/split input #"\n"))

(def initial-beam #{(str/index-of (first rows) "S")})

(defn gen-new-beams [at-index]
  [(inc at-index) (dec at-index)])

(defn row-value-with-index [row ind]
  [(get row ind) ind])

(defn process-row [[split-count beams] row]
  "Process a row and return the beams for the next row as well as a count
  of the number of splits that have occured."
  (let [values-at-beams-with-index (map #(row-value-with-index row %) beams)
        hit-splitter (map second (filter #(= \^ (first %)) values-at-beams-with-index))
        new-beams (apply hash-set (mapcat gen-new-beams hit-splitter))]
    [(+ split-count (count hit-splitter))
     (set/union new-beams (set/difference beams (apply hash-set hit-splitter)))]))

(defn part1 []
  (first (reduce process-row [0 initial-beam] (rest rows))))

(println (part1))

(def initial-beam-hash {(str/index-of (first rows) "S") 1})

(defn get-num-beams [beams ind]
  (get beams ind 0))

(defn in-range [row ind]
  (and (>= ind 0) (< ind (count row))))

(defn add-fn [old-value add-val]
  (if (nil? old-value) add-val (+ old-value add-val)))

(defn add-beams [beams new-beam]
  (update beams (:location new-beam) add-fn (:num-beams new-beam)))

(defn zero-beam [beams beam-index]
  (dissoc beams beam-index))

(defn current-beams-at [ind beams]
  (get beams ind 0))

(defn gen-new-beams2 [at-index beams]
  [{:location (inc at-index)
    :num-beams (current-beams-at at-index beams)}
   {:location (dec at-index)
    :num-beams (current-beams-at at-index beams)}])

(defn process-row2 [beams row]
  "Process a row and return the beams for the next row (including how many
  beams are at a given index)"
    (let [values-at-beams-with-index (map #(row-value-with-index row %) (keys beams))
        hit-splitter (map second (filter #(= \^ (first %)) values-at-beams-with-index))
        new-beams (filter #(in-range row (:location %)) (mapcat #(gen-new-beams2 % beams) hit-splitter))]
    (reduce add-beams (reduce zero-beam beams hit-splitter) new-beams)))

(defn part2 []
  (reduce + (vals (reduce process-row2 initial-beam-hash (rest rows)))))

(println (part2))
