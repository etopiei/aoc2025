(require '[clojure.string :as str])

;; (def input (slurp "test.txt"))
(def input (slurp "input.txt"))

(defn add-item-to-grid [grid [x y char]]
  (assoc grid [x y] char))

(def rows (str/split input #"\n"))

(defn get-items [x row]
  (map-indexed #(into [] [x % %2]) row))

(def items (apply concat (map-indexed get-items rows)))

(def grid (reduce add-item-to-grid {} items))

(defn get-neighbours [[x y] grid]
  [(get grid [(inc x) y])
   (get grid [(dec x) y])
   (get grid [x (inc y)])
   (get grid [x (dec y)])
   (get grid [(inc x) (inc y)])
   (get grid [(inc x) (dec y)])
   (get grid [(dec x) (dec y)])
   (get grid [(dec x) (inc y)])
   ])

(defn not-paper? [value]
  (not= \@ value))

(defn is-paper? [value]
  (= \@ value))


(defn part1 []
  (let [paper-items (filter #(is-paper? (get grid %)) (keys grid))
        num-paper-neighbours (map #(count (filter is-paper? (get-neighbours % grid))) paper-items)]
    (count (filter #(< % 4) num-paper-neighbours))))

(println (part1))

;; Now for part 2, we want to do the same count (find accesible paper items)
;; and once we have found some collection of rolls, we edit the grid to have 'non-paper' at
;; all the locations we found. Repeat this process until the 'accesible items' is zero
;; and return the number of items we removed total (need to count this as we go)

(defn get-removable-from-grid [grid]
   (let [paper-items (filter #(is-paper? (get grid %)) (keys grid))]
    (filter #(< (count (filter is-paper? (get-neighbours % grid))) 4) paper-items)))

(defn remove-from-grid [grid items-to-remove]
   (reduce #(assoc % %2 \.) grid items-to-remove))

(defn part2 []
  (loop [current-grid grid total-removed 0]
    (let [removable (get-removable-from-grid current-grid)]
      (if (zero? (count removable))
        total-removed 
        (recur (remove-from-grid current-grid removable) (+ total-removed (count removable)))))))

(println (part2))
