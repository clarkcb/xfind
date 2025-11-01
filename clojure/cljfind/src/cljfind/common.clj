(ns cljfind.common)

(defn log-msg [^String msg & msgs]
  (println msg)
  (doseq [m msgs] (println m)))

(defn log-error [^String err]
  (.println *err* (str "\nERROR: " err)))

(defn log-errors [errs]
  (doseq [e errs] (log-error e)))

(defn as-keyword [str-or-keyword]
  (if (keyword? str-or-keyword)
    str-or-keyword
    (keyword str-or-keyword)))

(defn as-string [str-or-keyword]
  (if (string? str-or-keyword)
    str-or-keyword
    (name str-or-keyword)))
