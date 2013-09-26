(ns hamlet.core
  (:use [clojure.java.io :only [reader]])
  (:use [clojure.string :only [join blank? split]])
  (:use [clojure.data.json :only [read-str write-str]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def force-update false)
(def cut-filename "./resources/cut.txt")
(def src-filename "./resources/hamlet.txt")

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

(def hard-to-regex-directions #{"Exit" "Exeunt" "Cock crows" "Enter Ghost" "Exit Ghost" "Re-enter Ghost" "Writing"})

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
    (= line "Ghost")         (mk-line line {:indent "        " :character "GHOST"})
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

(defn count-lines [text]
  (let [text-lines (filter :ln text)
        total-count (count text-lines)
        cut-count (count (filter #(:cut (:meta %)) text-lines))
        uncut-count (- total-count cut-count)]
    (str uncut-count " / " total-count " ("  (format "%.2f" (/ uncut-count total-count 0.01)) "%)")))

(defn write-out [text]
  (spit cut-filename
    (str
      "("
      (apply str (interpose "\n" (map pr-str text)))
      ")")))

(defn load-parsed []
  (if (and (not force-update) (file-exists cut-filename))
    (do
      (def parsed-text (read-string (slurp cut-filename)))
      (println "Read from file."))
    (do
      (def parsed-text (parse (file-line-seq src-filename)))
      (write-out parsed-text)
      (println "Re-processed from original file.")))
  (println (str "Total: " (count-lines parsed-text))))

(load-parsed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(def cut-text parsed-text)

(def css "<style>
.ln {
	width: 50px;
	display: inline-block;
}

.line {
	background-color: white;
}
.line:hover {
	background-color: rgb(240, 240, 240);
}

.text {
	margin-left: 50px;
	width: 700px;
	display: inline-block;
	color: rgb(0, 100, 0);
	font-weight: bold;
}

.cut {
	color: rgb(250, 200, 200);
	font-weight: normal;
}

.meta {
	margin-left: 0px;
	margin-right: 50px;
	width: 700px;
	display: inline-block;
}

.note {
  width: 500px;
}

</style>")

(def js "<script>

function send(data) {
	request = new XMLHttpRequest();
	request.open(\"POST\", \"data.handler\", true);
	request.setRequestHeader(\"data\", data);
	request.send();
}

function cut(line_number) {
	send(\"cut:\" + line_number);
	el = document.getElementById(line_number);
	el.classList.toggle(\"cut\");
}

function annotate(line_number) {
	el = document.getElementById(\"note-\" + line_number);
  send(\"note:\" + line_number + \":\" + el.value)
}

</script>")

(def head (str "<html><head>" css js "</head><body>"))
(def tail "</body></html>")


(defn proc-attrs [[k v]]
  (str " " (name k) "=" \" v \"))

(defn tag [tagname attrs & content]
  (str "<" tagname (apply str (map proc-attrs attrs)) ">" (apply str content) "</" tagname ">"))

(def span (partial tag "span"))
(def div (partial tag "div"))
(def input (partial tag "input"))

(defn web-format [line id]
  (let [line-type (if (:ln line) "text" "meta")
        is-cut (if (-> line :meta :cut) " cut" "")
        note (-> line :meta :note)
        line-class (str line-type is-cut)]
    (str (div {:class "line"}
           (span {:class "ln"} (:ln line))
           (span {:id id :class line-class} (:text line))
           (input {:value "cut" :type "submit" :onclick (str "cut(" id ");") } "")
           (input {:id (str "note-" id) :class "note" :value note :type "text" :onblur (str "annotate(" id ");") } "")
         ))))

(defn my-page []
  (str head (apply str (map web-format cut-text (range))) tail))

(defn cut-data [line]
  (let [meta (:meta line)
        cut? (:cut meta)
        cut (not cut?)]
    (assoc line :meta (assoc meta :cut cut))))

(defn note-data [line note]
  (let [meta (:meta line)]
    (assoc line :meta (assoc meta :note note))))

(defn cut-line [line-number]
  (def cut-text
    (concat
      (take line-number cut-text)
      [(cut-data (nth cut-text line-number))]
      (drop (inc line-number) cut-text)))
  (write-out cut-text)
  (println (str (count-lines (take (inc line-number) cut-text))
                " up to the current line. Total: "
                (count-lines cut-text))))

(defn note-line [line-number note]
  (def cut-text
    (concat
      (take line-number cut-text)
      [(note-data (nth cut-text line-number) note)]
      (drop (inc line-number) cut-text)))
  (write-out cut-text)
  (println (str "The note is: \"" note \")))

(defn handle [data]
  (cond
    (.startsWith data "cut:")
      (cut-line (read-string (last (split data #":"))))
    (.startsWith data "note:")
      (let [bits (split data #":")
            id (read-string (nth bits 1))
            note (if (= (count bits) 3) (nth bits 2) "")]
        (note-line id note))
    :else
      (println (str data "?"))))

(defn handler [request]
  (cond
    (= (request :uri) "/")
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (my-page)}
    (= (request :uri) "/data.handler")
      (do
        (handle (-> request :headers (get "data")))
        {:status 200})
    :else
      {:status 418}))
