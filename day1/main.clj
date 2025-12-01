(require '[clojure.string :as str])

(def input (slurp "input.txt"))

(def lines (str/split input #"\n"))

(defn make-op [inst]
  (if (= (first inst) \L)
    {:op - :by (Integer/parseInt (apply str (rest inst)))}
    {:op + :by (Integer/parseInt (apply str (rest inst)))}))

(def instructions (map make-op lines))

(defn new-val [current instruction]
  (mod ((:op instruction) current (:by instruction)) 100))

(defn is-zero-count [value]
  (if (zero? value) 1 0))

(defn apply-inst [acc instruction]
  (let [new-val (new-val (:current acc) instruction)]
    {:current new-val :zero_count (+ (:zero_count acc) (is-zero-count new-val))}))

(defn part1 []
  (:zero_count (reduce apply-inst {:current 50 :zero_count 0} instructions)))

(println (part1))

(defn full-rotations [spin-by] (quot spin-by 100))

(defn remain-check [start-val instruction]
  (let [spin-by (mod (:by instruction) 100)
        op (:op instruction)
        modified-start (if (and (zero? start-val) (= op -)) 100 start-val)
        after-spin (op modified-start spin-by)]
        (if (or (>= after-spin 100) (<= after-spin 0)) 1 0)))

(defn new-zeroes-from [start-val instruction]
  ;; Calculate how many times this went passed, or landed exactly on zero
  ;; To do this, we want the total number of full rotations (each full rotation necessarily includes a zero) + 1 iff the value it's moving mod 100 is greater/less than or equal to the current value
  (+ (full-rotations (:by instruction)) (remain-check start-val instruction)))

(defn apply-inst2 [acc instruction]
  (let [new-val (new-val (:current acc) instruction)]
    {:current new-val
     :zero_count (+ (:zero_count acc) (new-zeroes-from (:current acc) instruction))}))

(defn part2 []
  (:zero_count (reduce apply-inst2 {:current 50 :zero_count 0} instructions)))

(println (part2))
