(ns cljfind.common
  (:use [cljfind.color :only (BOLD_RED RESET)]))

(defn log-msg [^String msg & msgs]
  (println msg)
  (doseq [m msgs] (println m)))

(defn log-error
  ([^String err]
    (log-error err true))
  ([^String err, colorize]
    (if colorize
      (.println *err* (str "\n" BOLD_RED "ERROR: " err RESET))
      (.println *err* (str "\nERROR: " err)))))

(defn log-errors
  ([errs]
  (log-errors errs true))
  ([errs colorize]
  (doseq [e errs] (log-error e colorize))))

(defn as-keyword [str-or-keyword]
  (if (keyword? str-or-keyword)
    str-or-keyword
    (keyword str-or-keyword)))

(defn as-string [str-or-keyword]
  (if (string? str-or-keyword)
    str-or-keyword
    (name str-or-keyword)))
