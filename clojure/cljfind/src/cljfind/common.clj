(ns cljfind.common)

(defn log-msg [^String msg & msgs]
  (println msg)
  (doseq [m msgs] (println m)))

(defn log-error [^String err]
  (.println *err* (str "\nERROR: " err)))

(defn log-errors [errs]
  (doseq [e errs] (log-error e)))
