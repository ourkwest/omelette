(ns hamlet.core
  (:use [clojure.java.io :only [reader]])
  (:use [clojure.string :only [join blank? split]])
  (:use [clojure.data.json :only [read-str write-str]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def force-update false)
(def cut-filename "./resources/cut.txt")
(def src-filename "./resources/hamlet.txt")
(def wip-filename "./resources/wip.txt")


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
(def inline-direction #".*\[.*")

(def hard-to-regex-directions #{"Exit" "Exeunt" "Cock crows" "Enter Ghost" "Exit Ghost" "Re-enter Ghost"
                                "Writing" "They swear" "Reads"})

(def not-actually-directions #{"this machine is to him, HAMLET.'" "and more strange return. 'HAMLET.'"})

(defn mk-line [line more]
  (merge {:meta {} :text line :line-count 0} more))

(defn parse-line [line]
  (cond 
    (re-matches title line)  (mk-line line {:indent ""         :title "Hamlet"})
    (re-matches acts line)   (mk-line line {:indent "  "       :act line :scene nil :direction nil :character :nil})
    (re-matches scenes line) (mk-line line {:indent "    "     :scene (first (clojure.string/split line #"\.")) :direction nil :character nil :line-number 0})
    (or (and (re-matches names line) 
             (re-matches not-names line) 
             (not (re-matches inline-direction line))
             (not (contains? not-actually-directions line)))
        (contains? hard-to-regex-directions line))
                             (mk-line line {:indent "      "   :direction line})
    (and (re-matches names line) (not (re-matches not-names line)))
                             (mk-line line {:indent "        " :character line})
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
  (spit wip-filename
    (str
      "("
      (apply str (interpose "\n" (map pr-str text)))
      ")"))
  )

(defn load-parsed []
  (if (and (not force-update) (file-exists wip-filename))
    (do
      (def parsed-text (read-string (slurp wip-filename)))
      (println "Read from file."))
    (do
      (def parsed-text (parse (file-line-seq src-filename)))
      (write-out parsed-text)
      (println "Re-processed from original file.")))
  (println (str "Total: " (count-lines parsed-text))))

(load-parsed)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn finalise [x]
  (let [cut (-> x :meta :cut)
        note (-> x :meta :note)
        notey (if (empty? note) nil note)
        final (-> x :meta :final)
        text (x :text)
        new-final (or final notey (if cut nil text))
        type (cond
               (= (x :text) (x :act)) "A"
               (= (x :text) (x :scene)) "S"
               (= (x :text) (x :direction)) "D"
               (= (x :text) (x :character)) "C"
               :else "T")]
    (assoc-in (assoc-in x [:meta :final] new-final) [:meta :type] type)))


;(def parsed-text (map finalise parsed-text)) ; only done once
;(write-out parsed-text)

;(println (take 30 parsed-text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; line-number, direction, character, text (in), text (cut), notes


(def gdoc-indent "REPT(CHAR(160); 4)")
(defn gdoc-formula [& args] (str \= (join \& args)))
(defn gdoc-ref [column] (str "INDIRECT(\"R\"&ROW()&\"C" column "\")"))
(def gdoc-text (gdoc-formula (gdoc-ref 3) gdoc-indent (gdoc-ref 4) gdoc-indent (gdoc-ref 5)))


(defn csv-quote [x]
  (if x
    (str \" (.replaceAll x "\"" "\"\"") \")
    ""))

(defn line-to-csv [x]
  (join
    ","
    [
     (csv-quote (:text x))
     (:type (:meta x))
     (if (:cut (:meta x)) 1 0) 
     (:character x)
     (csv-quote (:final (:meta x)))
     "\n"
     ]))

;    (concat [(if (:cut (:meta x)) 1 0) (:line-number x)]
;      (cond
;        (= (:direction x) (:text x)) [(:direction x) "" ""]
;        (= (:character x) (:text x)) ["" (:character x) ""]
;        :else                        ["" "" (csv-quote (:text x))])
;      [(csv-quote (:note (:meta x))) gdoc-text "\n"])))

;;(println (apply str (map line-to-csv (take 35 parsed-text))))

(defn collect-scenes [coll line]
  (let [act (:act line)
        scene (:scene line)]
    (if (and act scene)
      (conj coll [(.replace (str act "_" scene ".csv") " " "_") act scene])
      coll)))

(defn collect-acts [coll line]
  (let [act (:act line)]
    (if act
      (conj coll [(.replace (str act ".csv") " " "_") act])
      coll)))

(def scenes (reduce collect-scenes #{} parsed-text))
(def acts (reduce collect-acts #{} parsed-text))

(defn by-scene [act scene]
  (fn [x]
    (and (= act (:act x)) (= scene (:scene x)))))

(defn by-act [act] (fn [x] (= act (:act x))))

(defn scene-to-csv [[filename act scene]]
  (spit filename (apply str (map line-to-csv (filter (by-scene act scene) parsed-text)))))

(defn act-to-csv [[filename act]]
  (spit filename (apply str (map line-to-csv (filter (by-act act) parsed-text)))))

;;(scene-to-csv ["delme.csv" "ACT I" "SCENE I"])
;(doall (map scene-to-csv scenes))
(doall (map act-to-csv acts))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def cut-text parsed-text)

(def css "<style>

html {
	color: RGB(200, 200, 200);
	background-color: black;
	font-family: arial;
}

.editme {
  border-left: 15px solid rgb(50, 50, 50);
  padding-left: 5px;
  margin-left: 5px;
}

.ln {
	width: 50px;
	display: inline-block;
}

.line {
}
.line:hover {
	background-color: rgb(40, 40, 40);
}

.text {
	margin-left: 50px;
	width: 450px;
	display: inline-block;
	color: rgb(100, 250, 100);
//	font-weight: bold;
}

.text.editme {
	margin-left: 50px;
}

.cut {
	color: rgb(150, 0, 0);
	font-weight: normal;
}

.tweak {
	color: rgb(200, 200, 0);
}

.meta {
	margin-left: 0px;
	margin-right: 50px;
	width: 450px;
	display: inline-block;
}

.note {
	width: 450px;
}

.final {
	width: 450px;
	background-color: #000;
	color: #fff;
}

.type {
  width: 15px;
}

input {
  margin-right: 10px;
  border: 1px solid grey;
	background-color: rgb(50, 50, 50);
	color: rgb(150, 150, 150);
}
input:hover {
  border: 1px solid white;
  background-color: rgb(0, 0, 0);
	color: white;
}
input:focus {
	background-color: rgb(50, 50, 50);
	color: rgb(250, 250, 250);
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
	send(\"cut|\" + line_number);
	el = document.getElementById(line_number);
	el.classList.toggle(\"cut\");
}

function annotate(line_number) {
	el = document.getElementById(\"note-\" + line_number);
  send(\"note|\" + line_number + \"|\" + el.value);
}

function askAnnotate(line_number) {
  el = document.getElementById(\"note-\" + line_number);
  oldValue = (el.firstChild && el.firstChild.data) || \"\";
  newValue = prompt(\"New note:\", oldValue);
  if (newValue && newValue != oldValue) {
    send(\"note|\" + line_number + \"|\" + newValue);
    el.innerHTML = newValue;
  }
}

function finalise(line_number) {
	el = document.getElementById(\"final-\" + line_number);
  send(\"final|\" + line_number + \"|\" + el.value);
}

function askFinalise(line_number) {
  el = document.getElementById(\"final-\" + line_number);
  oldValue = (el.firstChild && el.firstChild.data) || \"\";
  newValue = prompt(\"Final text:\", oldValue);
  if (newValue && newValue != oldValue) {
    send(\"final|\" + line_number + \"|\" + newValue);
    el.innerHTML = newValue;
  }
}

</script>")

(def head (str "<!DOCTYPE html><html><head>" css js "</head><body>"))
(def tail "</body></html>")


(defn proc-attrs [[k v]]
  (str " " (name k) "=" \" v \"))

(defn tag [tagname attrs & content]
  (str "<" tagname (apply str (map proc-attrs attrs)) ">" (apply str content) "</" tagname ">"))

(def span (partial tag "span"))
(def div (partial tag "div"))
(def input (partial tag "input"))

(defn web-format [line id]
  (let [text (-> line :text)
        note (-> line :meta :note)
        final (-> line :meta :final)
        type (-> line :meta :type)
        line-type (if (:ln line) "text" "meta")
        is-cut (if (-> line :meta :cut) " cut" "")
        is-tweaked (if (not= text final) " tweak" "")]
    (str (div {:class "line"}
           (span {:class "ln"} (:ln line))
           (span {:id id :class (str line-type is-cut)} (:text line))
           (input {:value "cut" :type "submit" :onclick (str "cut(" id ");") } "") ; :tabindex id :onmouseover "this.focus();"
           (span {:id (str "final-" id)
                  :class (str line-type is-cut is-tweaked " editme")
                  :onclick (str "askFinalise(" id ");")} (if (-> line :meta :cut) "" final))
           (span {:id (str "note-" id)
                  :class "note editme"
                  :onclick (str "askAnnotate(" id ");")} note)
;           (input {:id (str "type-" id) :class "type" :value type :type "text" :onblur (str "typify(" id ");") } "")
 ;          (input {:id (str "final-" id) :class "final" :value final :type "text" :onblur (str "finalise(" id ");") } "")
  ;         (input {:id (str "note-" id) :class "note" :value note :type "text" :onblur (str "annotate(" id ");") } "")
         ))))

(defn my-page []
  (str head (apply str (map web-format cut-text (range))) tail))

(defn cut-data [line]
  (let [meta (:meta line)
        cut? (:cut meta)]
    (if cut?
      (assoc-in (assoc-in line [:meta :cut] false) [:meta :final] (:text line))
      (assoc-in line [:meta :cut] true))
    ;(assoc line :meta (assoc meta :cut cut))
    ))

(defn note-data [line note]
  (let [meta (:meta line)]
    (assoc line :meta (assoc meta :note note))))

(defn final-data [line final]
  (let [meta (:meta line)]
    (assoc line :meta (assoc meta :final final)))) ;; should use assoc-in

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

(defn final-line [line-number final] ;; definite repetition here
  (def cut-text
    (concat
      (take line-number cut-text)
      [(final-data (nth cut-text line-number) final)]
      (drop (inc line-number) cut-text)))
  (write-out cut-text)
  (println (str "The final line is: \"" final \")))

(defn handle [data]
  (cond
    (.startsWith data "cut|")
      (cut-line (read-string (last (split data #"\|"))))
    (.startsWith data "note|")
      (let [bits (split data #"\|")
            id (read-string (nth bits 1))
            note (if (= (count bits) 3) (nth bits 2) "")]
        (note-line id note))
    (.startsWith data "final|")
      (let [bits (split data #"\|")
            id (read-string (nth bits 1))
            final (if (= (count bits) 3) (nth bits 2) "")]
        (final-line id final))
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
