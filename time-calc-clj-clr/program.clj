;Compilation notes:
; The vsClojure installer should have stored the core framework directory in the VSCLOJURE_RUNTIMES_DIR environment variable pointing to C:\Users\[username]\AppData\Local\Microsoft\VisualStudio\[version]\Extensions\[randomCharacters]\Runtimes\

(ns program
  (:require [clojure.string :only [split, split-lines] :as str])
;  (:import (System.Text.RegularExpressions Regex MatchEvaluator Match)              ; java.util.regex Pattern
;           clojure.lang.LazilyPersistentVector)
  (:gen-class))

;; I cannot determine how to correctly reference these functions from
;; clojure.string.
;(defn str/split
;  "Splits string on a regular expression.  Optional argument limit is
;  the maximum number of splits. Not lazy. Returns vector of the splits."
;  {:added "1.2"}
;  ([^String s ^Regex re]                                                   ;;; ^Pattern 
;     (LazilyPersistentVector/createOwning (.Split re s)))                  ;;; .split
;  ([^String s ^Regex re limit]                                             ;;; ^Pattern 
;     (LazilyPersistentVector/createOwning (.Split re s limit))))           ;;; .split
; 
;(defn str-split-lines
;  "Splits s on \\n or \\r\\n."
;  {:added "1.2"}
;  [^String s]
;  (str/split s #"\r?\n"))
;
(defrecord Task [start end details])

(defrecord TimePoint [hour min])

(defrecord TimeInterval [hours mins])

(defn task-duration
  "Calcluate the duration of a task."
  [task]
  (let [{start-hours :hour start-mins :min} (:start task)
        {end-hours :hour end-mins :min} (:end task)]
    (if (< end-mins start-mins)
      (TimeInterval. (- (- end-hours 1) start-hours)
                     (- (+ end-mins 60) start-mins))
      (TimeInterval. (- end-hours start-hours)
                     (- end-mins start-mins)))))

(defn add-interval
  "Adds two TimeInteval instances."
  [interval-1 interval-2]
  (let [hours-sum (+ (:hours interval-1) (:hours interval-2))
        minutes-sum (+ (:mins interval-1) (:mins interval-2))]
    (if (> minutes-sum 59)
      (TimeInterval. (+ 1 hours-sum) (- minutes-sum 60))
      (TimeInterval. hours-sum minutes-sum))))

(defn between? 
  "Is to-test in [lower, upper)?"
  [to-test lower upper]
  (and (>= to-test lower) (< to-test upper)))

(defn interval->decimal-interval
  "Converts a TimeInterval to a decimal value of hours."
  [{hours :hours mins :mins}]
  (cond (between? mins 0 8) hours
        (between? mins 8 23) (+ hours 0.25)
        (between? mins 23 38) (+ hours 0.5)
        (between? mins 39 54) (+ hours 0.75)
        (between? mins 53 60) (+ hours 1)))

(defn summarize-tasks
  "Summarize a sequence of tasks."
  [task-seq]
  (let [interval-map (reduce
                      #(assoc %1
                         (:details %2)
                         (add-interval (task-duration %2)
                                       (get %1 (:details %2)
                                            (TimeInterval. 0 0))))
                         {} task-seq)]
    (into {} (for [[d i] interval-map]
               [d (interval->decimal-interval i)]))))

(defn line->task
  "Convert a single line to a task."
  [a-line]
  (let [[start-text details] (str/split a-line #"[ \t]")
        start (TimePoint. (Int32/Parse (subs start-text 0 2))
                          (Int32/Parse (subs start-text 2)))
        end start]
    (Task. start end details)))

(defn zip
  "Equivalent of Python zip()."
  [& colls]
  (apply map vector colls))

(defn join-tasks
  "Creates a contiguous time range for tasks."
  [tasks]
  (let [zipped-tasks (zip tasks (drop 1 tasks))]
    (map #(Task. (:start (%1 0)) (:start (%1 1)) (:details (%1 0)))
         zipped-tasks)))

(defn lines->tasks
  "Convert a sequences of lines to tasks."
  [line-seq]
  (let [raw-tasks (map line->task line-seq)]
    (join-tasks raw-tasks)))

(defn read-all-lines
  "Read all lines from a file."
  [filename]
  (let [content (slurp filename :encoding "UTF-8")]
    (filter #(not (String/IsNullOrWhiteSpace %)) 
            (str/split-lines content))))
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [filename (if args
                   (first args)
                   "../../../time.txt")
        summary (summarize-tasks
                      (lines->tasks (read-all-lines
                                     filename)))]
    (doseq [[details duration]
            (sort-by first summary)]
      (println (format "%-8s %5.2f" details (float duration))))))

