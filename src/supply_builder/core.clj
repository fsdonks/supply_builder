(ns supply_builder.core
  (:gen-class)
  (:import [javax.swing JFrame JFileChooser])
  (:require
   [clojure.java [io :as io]]))
(set! *warn-on-reflection* true)
;; ======================================================================================
;; ======================================================================================



;; ======================================================================================
;; ===== FUNCTION TO READ FILE FORMAT FROM FILE =========================================
;; ======================================================================================

;; Record formats in time have the following form:
;; attribute-name1[\t]start-index[\t]length
;; attribute-name2[\t]start-index[\t]length
;; attribute-name3[\t]start-index[\t]length
;; ........................................
;; attribute-nameN[\t]start-index[\t]length

;; Returns coll attribute names, start-indices, and lengths
;; Takes optional delimiter argument for when format file is not tab delimited
(defn read-format [filename & del]
  (with-open [r (io/reader filename)]
    (doall
     (map #(vec [(keyword (first %)) (read-string (second %)) (read-string (last %))])
          (for [line (line-seq r) :let [del (if del del "\t")]]
            (clojure.string/split line (re-pattern del)))))))
;; ======================================================================================
;; ======================================================================================



;; ======================================================================================
;; ===== FILE FORMATS ===================================================================
;; ======================================================================================
(def format340
  '([:rec-count 1 1] [:count 1 1] [:type-force 2 1] [:compo 3 1] [:uic 4 6] [:date 10 6] [:year 10 2] [:actco 16 1] [:amscos 17 10] [:amsco 17 8] [:asgmt 27 2] [:macom 27 2] [:authr 29 15] [:unit_name 29 15] [:brnch 44 2] [:carss 46 2] [:cars 46 2] [:ccnum 48 6] [:mdepc 54 4] [:dampl 58 5] [:darpl 58 5] [:pruic-1 63 3] [:fad 67 1] [:mbprd 68 3] [:dscmp 71 2] [:elseq 73 2] [:ffdbUTC 75 5] [:DMC-1 75 4] [:forco 79 1] [:FPI 80 1] [:uxref 81 7] [:unlocco 88 3] [:mbcmd 94 2] [:mbloc 96 3] [:mbsta 101 9] [:ntref 113 2] [:forcres 113 2] [:adcco 119 6] [:ntpsn 120 5] [:phase 127 1] [:mrobco 129 4] [:repco 129 1] [:robco 130 3] [:src 134 9] [:alo 143 1] [:src2 134 2] [:src1-12 134 12] [:src11-13 144 3] [:src1-13 134 13] [:variant 146 1] [:staco 147 5] [:stnnm 154 9] [:docno 168 10] [:commandcode 174 2] [:tpsn 180 5] [:tpsn3 180 3] [:typco 186 1] [:dp99 188 4] [:robc4 188 1] [:ROC 189 3] [:unmbr 194 4] [:untds 204 21] [:vchnr 225 3] [:ffdbulc 225 3] [:auoff 228 5] [:auwof 233 5] [:auenl 238 5] [:auagr 243 5] [:stoff 278 5] [:stwof 283 5] [:stenl 288 5] [:stagr 293 5] [:tdate 303 8] [:newuic 312 6] [:dpmnt 328 4] [:udp99 328 4] [:pruic-2 336 3] [:century 339 2] [:record 1 340]))

  ;(read-format "./resources/340-format.txt"))

;; This is just the 340 format with the last 31 charators removed.
;; Need to change this file to the correct format for 309 records
(def format309
  '([:compo 0 1] [:uic 1 6] [:edate 7 8] [:century 7 2] [:date1 9 6] [:actco 23 1] [:phase 24 1] [:typco 25 1] [:dscmp 26 2] [:asgmt 28 2] [:ROC 30 3] [:src 33 9] [:alo 42 1] [:src11_12 43 2] [:src1_12 33 12] [:carss 45 2] [:unmbr 47 4] [:brnch 51 2] [:ffdbulc 53 3] [:pruic_2 53 3] [:untds 56 18] [:ccnum 74 6] [:docno 80 11] [:staco 91 5] [:stnnm 96 9] [:state 106 2] [:locco 105 3] [:tpsn 108 5] [:elseq 113 2] [:forco 115 1] [:multi_ams 115 1] [:mbcmd 116 2] [:mbloc 118 3] [:mbsta 121 9] [:repco 128 1] [:mbprd 130 3] [:dpmnt 133 4] [:adcco 137 6] [:ntref 143 2] [:mrobco 145 4] [:robc4 148 1] [:fad 149 1] [:dampl 150 5] [:FPI 155 1] [:amsco 156 9] [:mdepc 165 4] [:mdep 165 4] [:auoff 169 5] [:auwof 174 5] [:auenl 179 5] [:auagr 184 5] [:stoff 189 5] [:stwof 194 5] [:stenl 199 5] [:stagr 204 5] [:tdate 15 8] [:rmark 209 100] [:authr 209 100]))


  ;;(read-format "./resources/309-format.txt"))

;; Determines the file format based on the length of the first record in file
;; If not in 340 or 309 format, throws exception
(defn get-format [filename]
  (let [c (with-open [r (clojure.java.io/reader filename)]
            (count (first (line-seq r))))]
    (if (= c 340) format340
        (if (= c 309) format309
            (throw (Exception. (str "No valid file format: " c))))))) 
;; ======================================================================================
;; ======================================================================================



;; ======================================================================================
;; ===== FUNCTION TO FORMAT RAW FILE ====================================================
;; ======================================================================================
;; ===== OLD ============================================================================
;; Reads line from file, formats it using file format, then writes line to output file
;; File format has to be a coll where each item is a coll
;; Where the first element is the attribute name,
;;       the second element is the starting index integer,
;;   and the the last element is the length of the attribute
(defn format-output [filename outfile file-format]
  (with-open [r (clojure.java.io/reader filename) w (clojure.java.io/writer outfile)]
    (doseq [h (map first file-format)] (.write w (str h "\t"))) (.write w "\n")
    (doseq [line (line-seq r)]
      (doseq [field (for [k file-format]
                      (subs line (second k) (+ (second k) (last k))))]
        (.write w (str field)) (.write w "\t")) (.write w "\n"))))
;; ======================================================================================
;; ======================================================================================



;; ======================================================================================
;; ===== FUNCTION TO FORMAT RAW SUPPLY FILE TO MARATHON SUPPLY ==========================
;; ======================================================================================
;; Gets value of field from substring of record using format and key
(defn get-field [line format key]
  (let [v (first (filter #(= key (first %)) format))]
    (subs line (second v) (+ (second v) (last v)))))

;; Sorts by key from record then partitions by the same key
(defn break-by [recs fileformat key]
  (partition-by #(get-field % fileformat key)
                (sort-by #(get-field % fileformat key) recs)))

;; Gets date from inputs file name. Assumes input file has the SAMAS naming convention
(defn get-date [infile]
  (read-string
   (second
    (clojure.string/split
     (last (clojure.string/split infile #"\\|/")) #" "))))

(def compos {"1" "AC"
             "2" "NG"
             "3" "RC"})

;; Build supply file.
;; This method is slow, takes ~10 secs to generate a supply file
;; Can speed up by using 
(defn format-out [filename outfile fformat]
  (with-open [r (clojure.java.io/reader filename) w (clojure.java.io/writer outfile)]
    (doseq [k ["Type" "Enabled" "Quantity" "SRC" "Component"	"OITitle" "Name" "Behavior"
               "CycleTime" "Policy" "Tags" "SpawnTime" "Location" "Position" "Original" "" "Strength"
               "Unit Groupings" "Low Density Supply"]]
      (.write w (str k "\t")))
    (.write w "\r\n")
    (let [lines (filter #(>= (read-string (get-field % fformat :edate))) (line-seq r))
          date (get-date filename)]
      (doseq [src (break-by lines fformat :src)]
        (doseq [compo (break-by src fformat :compo)
                :let [quantity (count (into #{} (map #(get-field % fformat :uic) compo)))]]
          (doseq [v ["SupplyRecord" ;type
                     "TRUE" ;enabled
                     (str quantity) ;;quantity
                     (get-field (last compo) fformat :src) ;;src
                     (get compos (get-field (last compo) fformat :compo)) ;;compo
                     (get-field (last compo) fformat :untds) ;; OITitle
                     "AUTO" ;;name
                     "AUTO" ;;behavior
                     "0" ;;cycle time
                     "AUTO" ;;policy
                     "AUTO" ;;tags
                     "0" ;;spawn time
                     "AUTO" ;;location
                     "AUTO" ;;position
                     "TRUE" ;; original
                     "" ;; empty col in original file (???)
                     (subs (last compo) 204 209)  ;; strength
                     (get-field (last compo) fformat :brnch) ;; unit grouping
                     ""]] ;;low densisty
            (.write w (str v "\t")))
          (.write w "\r\n"))))))
;; ======================================================================================
;; ======================================================================================



;; ======================================================================================
;; ===== MAIN METHOD FOR JAR FILE =======================================================
;; ======================================================================================


;; Opens file select window and returns list of file paths
(defn choose-file [title & dir-only]
  (let [f (javax.swing.JFrame.)
        c (javax.swing.JFileChooser.)]
    (.add f c)
    (.setDialogTitle c title)
    (when dir-only (.setFileSelectionMode c JFileChooser/DIRECTORIES_ONLY))
    (.setMultiSelectionEnabled c true)
    (let [x (.showOpenDialog c f)]
      (if  (zero? x)
        (map #(.getPath ^java.io.File %) (.getSelectedFiles c))
        (println "No file selected.")))))

;; Function to build arguments for format-output
;; If no arguments are given, file selection menu will open
;; for both input file path and output directory
;; The output filename will be FORMATTED_[input file name]
(defn ->format-output []
  (let [input-file (choose-file "Input file")
        out-dir (choose-file "Output directory" true)
        out (str (first out-dir) "/FORMATTED_"
                 (last (clojure.string/split (first input-file) #"\\|/")))]
    [(first input-file) out]))
;; ======================================================================================
;; ======================================================================================

;; Builds supply file given date. Will open file-select menu to select input file and output directory
(defn ->make-supply []
  (let [args (->format-output)]
    (time
     (format-out (first args) (last args) (get-format (first args))))))


(comment ;; when moving to standalone jar file
;; The arguments given should be the following order: 
;; 1) raw file
;; 2) output file
;; If no arguments are give, will create arguments using ->format-output function
(defn -main [& args] 
  (let [args (if args args (->format-output))]
    (format-output (first args) (last args) (get-format (first args)))
    (try
      (println "DONE.")
      (System/exit 0) ;; Force jar to exit when done, sometimes hangs
      (catch Exception e (println e)))))
)

(defn -main [& args]
  (->make-supply)
  (try
    (println "Done.")
    (System/exit 0) ;; For jar to close when done, sometimes hangs
    (catch Exception e (println e))))
