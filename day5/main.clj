
(require '[clojure.string :as str])

;; (def input (slurp "test.txt"))
(def input (slurp "input.txt"))

(def ranges-list (first (str/split input #"\n\n")))
(def ingredient-ids (second (str/split input #"\n\n")))

(defn parse-range [range-str]
  (map parse-long (str/split range-str #"-")))

(def ingredients (map parse-long (str/split ingredient-ids #"\n")))
(def ranges (map parse-range (str/split ranges-list #"\n")))

(defn x-in-range [x range]
  (and (>= x (first range)) (<= x (second range))))

(defn ingredient-in-any-range [ingredient]
  (some #(x-in-range ingredient %) ranges))

(defn part1 []
  (count (filter ingredient-in-any-range ingredients)))

(println (part1))

(defn trim-range [[new-start new-end] [existing-start existing-end]]
   "Return a range that has no parts overlapping with the existing range
   (ranges are inclusive)"
  ;; First check if we have nil already
  ;; Then there are five cases to consider
  ;; 1. No overlap, return as is
  ;; 2. range is completely inside, return [nil nil]
  ;; 3. new-end is between existing-start and existing-end, move new-end to existing-start - 1
  ;; 4. new-start is between existing-start and existing-end, move new-start to existing-end + 1
  ;; 5. existing is completely covered by this new range, this should not happen as we add in order of size of range
  (if (and (nil? new-start) (nil? new-end)) [new-start new-end]
      (if (nil? existing-start) [new-start new-end]
          (if (or (> new-start existing-end) (< new-end existing-start)) [new-start new-end]
              (if (and (>= new-start existing-start) (<= new-end existing-end)) [nil nil]
                  (if (and (>= new-end existing-start) (<= new-end existing-end)) [new-start (dec existing-start)]
                      (if (and (>= new-start existing-start) (<= new-start existing-end)) [(inc existing-end) new-end]
                          (println "BAD!" new-start new-end existing-start existing-end))))))))

(defn reduce-range [ranges new-range]
   "Reduce a range to only the parts that don't overlap with existing
   ranges. Returns a new range list with the new range appended"
  (conj ranges (reduce trim-range new-range ranges)))

(defn nums-in-range [[range-start range-end]]
  (inc (- range-end range-start)))

(defn part2 []
  (let [sorted-ranges (sort #(compare (nums-in-range %2) (nums-in-range %)) ranges)
        non-overlapped-ranges (remove #(nil? (first %)) (reduce reduce-range [] sorted-ranges))]
    (reduce + (map nums-in-range non-overlapped-ranges))))

(println (part2))
