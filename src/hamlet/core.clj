(ns hamlet.core
  (:use [clojure.java.io :only [reader]])
  (:use [clojure.string :only [join]]))



(defn file-line-seq [filename]
  (with-open [r (reader filename)]
    (doall (line-seq r))))

(def title #".*The Tragedy of Hamlet.*")
(def names #".*[A-Z]{2}.*")
(def not-names #".*[a-z]{1}.*")
(def scenes #".*\bSCENE\b.*")
(def acts #".*\bACT\b.*")

(defn to-field [n line]
  (concat (take n (repeat "  ")) [line] (take (- 5 n) (repeat "  "))))

(defn split [line]
  (cond
    (re-matches title line)  (to-field 0 line)
    (re-matches acts line)   (to-field 1 line)
    (re-matches scenes line) (to-field 2 line)
    (and (re-matches names line) (re-matches not-names line))
                             (to-field 3 line)
    (re-matches names line)  (to-field 4 line)
    :else                    (to-field 5 line)
  ))

(defn file-exists [filename]
  (let [f (java.io.File. filename)]
    (println (.getAbsolutePath f))
    (.exists f)))

(let [f-name "./resources/processed.txt"]
  (if (file-exists f-name)
    (do
      (def processed-text (read-string (slurp f-name)))
      (println "Read from file."))
    (do
      (def processed-text (map split (file-line-seq "./resources/hamlet.txt")))
      (spit f-name (pr-str processed-text))
      (println "Re-processed from original file."))))
      
(println (str "Sample: \n" (join "\n" (map (partial apply str) (take 16 processed-text)))))
 
(println (str "Lines: " (count (filter #(not (clojure.string/blank? (nth % 5))) processed-text))))
 
;;(spit "./resources/processed.txt" processed-text)

;; use read-string and spit to read and write clojure data structures to a file.
