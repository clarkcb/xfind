(ns cljfind.cljfind
  (:require [cljfind.findsettings])
  (:require [cljfind.finder])
  (:import [cljfind.findsettings FindSettings])
  (:import [cljfind.finder Finder])
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljfind.finder]
;        [cljfind.finder :only
;          (->Finder find-files print-matching-dirs print-matching-files)]
        [cljfind.findoptions :only (settings-from-args usage)])
  (:gen-class))

(defn -main
  "This will be the main function for cljfind"
  [& args]
  (let [[^FindSettings settings errs] (settings-from-args args)]
    (if (empty? errs)
      (do
        (if (:debug settings) (log-msg settings))
        (if (:print-usage settings) (usage))
        (let [finder (->Finder settings)
              [files errs] (find-files finder)]
          (if (empty? errs)
            (do
              (if (:print-dirs settings) (print-matching-dirs finder files))
              (if (:print-files settings) (print-matching-files finder files)))
            (do
              (log-errors errs (:colorize settings))
              (usage)))))
      (do
        (log-errors errs true)
        (usage)))))
