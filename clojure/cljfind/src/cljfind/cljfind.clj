(ns cljfind.cljfind
  (:require [cljfind.findsettings])
  (:import [cljfind.findsettings FindSettings])
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljfind.finder :only
          (find-files print-matching-dirs print-matching-files)]
        [cljfind.findoptions :only (settings-from-args usage)])
  (:gen-class))

(defn -main
  "This will be the main function for cljfind"
  [& args]
  (let [[^FindSettings settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:print-usage settings) (usage))
        (let [[files errs] (find-files settings)]
          (if (empty? errs)
            (do
              (if (:print-dirs settings) (print-matching-dirs files))
              (if (:print-files settings) (print-matching-files files)))
            (do
              (log-errors errs)
              (usage)))))
      (do
        (log-errors errs)
        (usage)))))
