(ns hamlet.core
  (:use [clojure.java.io :only [reader]])
  (:use [clojure.string :only [join blank?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def force-update true)

(defn file-line-seq [filename]
  (with-open [r (reader filename)]
    (doall (line-seq r))))

(defn file-exists [filename]
  (let [f (java.io.File. filename)]
    (println (.getAbsolutePath f))
    (.exists f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def title #".*The Tragedy of Hamlet.*")
(def names #".*[A-Z]{2}.*")
(def not-names #".*[a-z]{1}.*")
(def scenes #".*\bSCENE\b.*")
(def acts #".*\bACT\b.*")

(def hard-to-regex-directions #{"Exit" "Exeunt" "Cock crows" "Enter Ghost" "Exit Ghost" "Re-enter Ghost"})

(defn mk-line [line more]
  (merge {:meta {} :text line :line-count 0} more))

(defn parse-line [line]
  (cond 
    (re-matches title line)  (mk-line line {:indent ""         :title "Hamlet"})
    (re-matches acts line)   (mk-line line {:indent "  "       :act line :scene nil :direction nil :character :nil})
    (re-matches scenes line) (mk-line line {:indent "    "     :scene (first (clojure.string/split line #"\.")) :direction nil :character nil :line-number 0})
    (or (and (re-matches names line) (re-matches not-names line)) (contains? hard-to-regex-directions line))
                             (mk-line line {:indent "      "   :direction line})
    (re-matches names line)  (mk-line line {:indent "        " :character line})
    (blank? line)            (mk-line line {:indent "          "})
    :else                    (mk-line line {:indent "          " :line-count 1}) ))

(defn parse-fill [filled line]
  (let [prev (last filled)
        this (parse-line line)
        prev-line (if prev (:line-number prev) 0)
	this-line (+ (:line-count this) prev-line)
        this-no (if (= 0 (:line-count this)) nil this-line)] ;; (* (:line-count this) this-line)]
    (conj filled (merge prev {:line-number this-line :ln this-no} this))))

(defn parse [lines]
  (reduce parse-fill [] lines))

(defn load-parsed []
  (let [f-name "./resources/parsed.txt"]
    (if (and (not force-update) (file-exists f-name))
      (do
        (def parsed-text (read-string (slurp f-name)))
        (println "Read from file."))
      (do
        (def parsed-text (parse (file-line-seq "./resources/hamlet.txt")))
        (spit f-name (pr-str parsed-text))
        (println "Re-processed from original file.")))))

(load-parsed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-line [act scene line]
  (count
    (take-while
      #(not (and (= act (:act %))
                 (= scene (:scene %))
                 (= line (or (:ln %) 0))
             ))
      parsed-text)))

(def reset-colour "\033[0m")
(defn green [text] (str "\033[1;32m" text reset-colour))

(defn format-line [n line]
  (let [colour-formatter (if (= (:ln line) n) green identity)]
    (colour-formatter (str (:ln line) (:indent line) (:text line)))))

(defn show-line [act scene line]
  (let [n (find-line act scene line)]
    (println
      (join "\n"
            (map #(format-line line %)
                 (take 20 (drop (- n 10) parsed-text)))))))

(show-line "ACT II" "SCENE II" 50)


