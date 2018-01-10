(ns supply_builder.supply)

(def compos {"1" "AC" "2" "NG" "3" "RC"})

(defn read-header [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (let [header (clojure.string/split (first (line-seq r)) (re-pattern "\t"))]
      (zipmap header (range (count header))))))

(defn line->map [header line] (zipmap (keys header) (map #(nth line %) (take (count line) (vals header)))))
(defn lines->map [header lines] (map #(line->map header %) lines))

(defn group-by-baseSRC [m] (zipmap (set (map #(get % "SRC BL") m)) (for [s (set (map #(get % "SRC BL") m))] (filter #(= s (get % "SRC BL")) m))))
    
(defn reduce-baseSRC [m]
  (let [reduce-q (fn [q n] (reduce + (map #(read-string %) (filter #(not= "" %) (map #(get % n) q)))))]
    (for [q (vals (group-by-baseSRC m))]
      (for [c (keys compos)]
        ["Supply Record" "TRUE" (str (reduce-q q c)) (get (first q) "SRC BL") (get compos c) (get (first q) "UNTDS") "Auto" "Auto" "0" "Auto" "Auto" "0" "Auto" "Auto"]))))
         ;0 Type 1 Enabled 2 Quantity 3 SRC 4 Component 5 OITitle 6 Name 7 Behavoir 8 CycleTime 9 Policy 10 Tags 11 Spwantime 12 Location 13 Position

(defn build-supply [filename outfile]
  (let [header (read-header filename)]
    (with-open [r (clojure.java.io/reader filename) w (clojure.java.io/writer outfile)]
      (doseq [h ["Type" "Enabled" "Quantity" "SRC" "Component" "OITitle" "Name" "Behavior" "CycleTime" "Policy" "Tags" "Spawntime" "Location" "Position"]]
        (.write w (str h "\t"))) (.write w "\n")
      (doseq [line (filter #(not= "0" (nth % 2)) (apply concat (reduce-baseSRC (lines->map header (drop 1 (map #(clojure.string/split % (re-pattern "\t")) (line-seq r)))))))]
        (doseq [v line]
          (.write w (str v "\t")))
        (.write w "\n")))))

(def test-file "C:\\Users\\michael.m.pavlak\\Desktop\\taa_test_data\\supply_builder\\ManySRCs\\Input\\SupplyManySRCs.txt")

