(ns cljfind.cljfind
  (:gen-class)
  (:use [cljfind.common :only (log-msg log-errors)]
        [cljfind.finder :only
          (find print-find-results print-matching-dirs print-matching-files
            print-matching-lines)]
        [cljfind.findoptions :only (settings-from-args usage)]))

(defn -main
  "This will be the main function for cljfind"
  [& args]
  (let [[settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:printusage settings) (usage))
        (let [errs (find settings)]
          (if (empty? errs)
            (do
              (if (:printresults settings) (print-find-results settings))
              (if (:listdirs settings) (print-matching-dirs))
              (if (:listfiles settings) (print-matching-files))
              (if (:listlines settings) (print-matching-lines settings)))
            (do
              (log-errors errs)
              (usage)))))
      (do
        (log-errors errs)
        (usage)))))
