(require '[clojure.string :as str])
(require '[clojure.math.combinatorics :as combo])
(require '[clojure.math :as math])

(def input (slurp "input.txt"))
(def closest-take 1000)
;(def input (slurp "test.txt"))
;(def closest-take 10)

(def lines (str/split input #"\n"))

(defn make-junction [line]
  (map parse-long (str/split line #",")))

(def junctions (map make-junction lines))

(def pairs (combo/combinations junctions 2))

(defn distance-between [[x1 y1 z1] [x2 y2 z2]]
    (math/sqrt (+ (math/pow (- x1 x2) 2) (math/pow (- y1 y2) 2) (math/pow (- z1 z2) 2))))

(def sorted-pairs (sort #(compare (apply distance-between %) (apply distance-between %2)) pairs))

(def closest-pairs (take closest-take sorted-pairs))

(defn find-root [junction circuit-map join-map]
  (if (nil? (circuit-map junction))
    (if (nil? (join-map junction)) nil (find-root (join-map junction) circuit-map join-map)) junction))

(defn merge-roots [root-a root-b circuit-map join-map]
  (let [old-root-size (circuit-map root-b)]
    [(-> circuit-map
         (dissoc root-b)
         (update root-a #(+ old-root-size %)))
     (assoc join-map root-b root-a)]))

(defn join
  "Takes two circuits to join, the cirucit size map and the 'join' map.
   Returns the modified circuit size and join map after joining the junctions given."
  [[circuit-size-map join-map] [junction-a junction-b]]
    (let [root-a (find-root junction-a circuit-size-map join-map)
        root-b (find-root junction-b circuit-size-map join-map)
        a-is-root (= root-a junction-a)
        b-is-root (= root-b junction-b)]
    (cond
      (and (nil? root-a) (nil? root-b)) [(assoc circuit-size-map junction-a 2) (assoc join-map junction-b junction-a)]
      (= root-a root-b) [circuit-size-map join-map]
      (nil? root-b) [(update circuit-size-map root-a inc) (assoc join-map junction-b root-a)]
      (nil? root-a) [(update circuit-size-map root-b inc) (assoc join-map junction-a root-b)]
      :else (merge-roots root-a root-b circuit-size-map join-map)
      )))

(defn part1 []
  (apply * (take 3 (sort #(compare %2 %) (vals (first (reduce join [{} {}] closest-pairs)))))))

(println (part1))

(defn is-done? [circuit-size-map]
  (and (= (count lines) (first (vals circuit-size-map))) (= 1 (count circuit-size-map))))

(defn part2 []
    (apply * (map first 
                (loop [circuit-size-map {}
                       join-map {}
                       pairs sorted-pairs
                       last-pair nil]
                  (if (is-done? circuit-size-map)
                    last-pair
                    (let [[new-circuit-size-map new-join-map] (join [circuit-size-map join-map] (first pairs))] 
                      (recur new-circuit-size-map new-join-map (rest pairs) (first pairs))))))))

(println (part2))
