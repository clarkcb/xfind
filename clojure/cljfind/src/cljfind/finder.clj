;;; find.clj: Recursive file find utility

;; by Cary Clark
;; April 10, 2010

(ns cljfind.finder
  #^{:author "Cary Clark",
     :doc "Recursive file find utility"}
  (:import (java.io File)
           (java.util.jar JarFile)
           (java.util.zip ZipFile))
  (:use [clojure.java.io :only (file reader)]
        [clojure.string :as str :only (join trim upper-case)]
        [cljfind.common :only (log-msg)]
        [cljfind.filetypes :only (archive-file? get-filetype)]
        [cljfind.fileutil :only
          (get-ext get-files-in-directory get-name hidden-dir? hidden-file?
            is-dot-dir?)]
        [cljfind.findfile :only (new-find-file find-file-path)]
        [cljfind.findresult :only
          (->FindResult find-result-to-string)]))

; ref to contain the seq of FindResult records
(def find-results (ref []))

(defn save-find-result
  "Saves a FindResult to the find-results vector ref"
  [r]
  (dosync
    (alter find-results conj r)))

(defn is-find-dir? [d settings]
  (or
    (is-dot-dir? (get-name d))
    (and
      (or
        (not (:excludehidden settings))
        (not (hidden-dir? d)))
      (or
       (empty? (:in-dirpatterns settings))
        (some #(re-find % (.getPath d)) (:in-dirpatterns settings)))
      (or
       (empty? (:out-dirpatterns settings))
        (not-any? #(re-find % (.getPath d)) (:out-dirpatterns settings))))))

(defn print-find-result [r settings]
  (log-msg (find-result-to-string r settings)))

(defn print-find-results [settings]
  (log-msg (format "\nFind results (%d):" (count (deref find-results))))
  (doseq [r (deref find-results)] (print-find-result r settings)))

(defn get-matching-dirs []
  (sort (distinct (map #(.getParent (:file %)) (deref find-results)))))

(defn print-matching-dirs []
  (let [dirs (get-matching-dirs)]
    (log-msg (format "\nDirectories with matches (%d):" (count dirs)))
    (doseq [d dirs] (log-msg d))))

(defn get-matching-files []
  (sort (distinct (map #(.getPath (:file %)) (deref find-results)))))

(defn print-matching-files []
  (let [files (get-matching-files)]
    (log-msg (format "\nFiles with matches (%d):" (count files)))
    (doseq [f files] (log-msg f))))

(defn get-matching-lines [settings]
  (let [lines (sort-by str/upper-case (map #(str/trim (:line %)) (deref find-results)))]
    (if (:uniquelines settings)
      (distinct lines)
      lines)))

(defn print-matching-lines [settings]
  (let [lines (get-matching-lines settings)]
    (log-msg
      (if (:uniquelines settings)
        (format "\nUnique lines with matches (%d):" (count lines))
        (format "\nLines with matches (%d):" (count lines))))
    (doseq [l lines] (log-msg l))))

(defn validate-settings [settings]
  (let [startpath (:startpath settings)
        startdir (if startpath (file startpath) nil)
        tests [(fn [ss] (if (not startpath) "Startpath not defined" nil))
               (fn [ss] (if (or (not startdir) (not (.exists startdir))) "Startpath not found" nil))
               (fn [ss] (if (and startdir (not (.canRead startdir))) "Startpath not readable" nil))
               (fn [ss] (if (empty? (:findpatterns ss)) "No find patterns defined" nil))
               (fn [ss]
                 (if
                   (not
                     (=
                       (try
                         (java.nio.charset.Charset/forName (:textfileencoding ss))
                         (catch IllegalArgumentException e nil))
                       nil)
                   ) nil (format "Invalid encoding: %s" (:textfileencoding ss))))
               (fn [ss] (if (< (:linesafter ss) 0) "Invalid linesafter" nil))
               (fn [ss] (if (< (:linesbefore ss) 0) "Invalid linesbefore" nil))
               (fn [ss] (if (< (:maxlinelength ss) 0) "Invalid maxlinelength" nil))
              ]
       ]
    (take 1 (filter #(not (= % nil)) (map #(% settings) tests)))))

(defn is-archive-find-file? [f settings]
  (and
    (or
     (empty? (:in-archiveextensions settings))
     (some #(= % (get-ext f)) (:in-archiveextensions settings)))
    (or
     (empty? (:out-archiveextensions settings))
     (not-any? #(= % (get-ext f)) (:out-archiveextensions settings)))
    (or
     (empty? (:in-archivefilepatterns settings))
     (some #(re-find % (.getName f)) (:in-archivefilepatterns settings)))
    (or
     (empty? (:out-archivefilepatterns settings))
     (not-any? #(re-find % (.getName f)) (:out-archivefilepatterns settings)))))

(defn is-find-file? [f settings]
  (and
    (or
     (empty? (:in-extensions settings))
     (some #(= % (get-ext f)) (:in-extensions settings)))
    (or
     (empty? (:out-extensions settings))
     (not-any? #(= % (get-ext f)) (:out-extensions settings)))
    (or
     (empty? (:in-filepatterns settings))
     (some #(re-find % (.getName f)) (:in-filepatterns settings)))
    (or
     (empty? (:out-filepatterns settings))
     (not-any? #(re-find % (.getName f)) (:out-filepatterns settings)))
    (or
     (empty? (:in-filetypes settings))
     (contains? (:in-filetypes settings) (get-filetype f)))
    (or
     (empty? (:out-filetypes settings))
     (not (contains? (:out-filetypes settings) (get-filetype f))))))

(defn filter-file? [f settings]
  (and
    (or
      (not (hidden-file? f))
      (not (:excludehidden settings)))
    (if (archive-file? f)
      (and
        (:findarchives settings)
        (is-archive-find-file? f settings))
      (and
        (not (:archivesonly settings))
        (is-find-file? f settings)))))

(defn get-find-files [settings]
  (let [startdir (file (:startpath settings))]
    (if (:recursive settings)
      (vec
       (map
        #(new-find-file % (get-filetype %))
        (filter #(filter-file? % settings) (filter #(.isFile %) (file-seq startdir)))))
      (vec
       (map
        #(new-find-file % (get-filetype %))
        (filter #(filter-file? % settings) (filter #(.isFile %) (.listFiles startdir))))))))

(defn find-archive-file [f settings]
  (if (:verbose settings)
    (log-msg (format "Finding archive file %s" f))))

(defn find-binary-string-for-pattern
  ([b p settings]
    (let [m (re-matcher p b)]
      (if (.find m 0)
        (find-binary-string-for-pattern b m 0 settings)
        [])))
  ([b m i settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              result (->FindResult
                       (.pattern m)
                       nil
                       0
                       (+ startmatchindex 1)
                       (+ endmatchindex 1)
                       ""
                       []
                       [])]
          (if (:firstmatch settings)
            [result]
            (concat [result] (find-binary-string-for-pattern b m
              endmatchindex settings)))))
      [])))

(defn find-binary-string [b settings]
  (if (:debug settings)
    (log-msg "Finding binary string"))
  (apply concat
    (map #(find-binary-string-for-pattern b % settings) (:findpatterns settings))))

(defn find-binary-file [sf settings]
  (if (:verbose settings)
    (log-msg (format "Finding binary file %s" (find-file-path sf))))
  (let [contents (slurp (:file sf) :encoding "ISO-8859-1") ; use single-byte enc to avoid corruption
        find-results (find-binary-string contents settings)
        with-file-results (map #(assoc-in % [:file] sf) find-results)]
    (doseq [r with-file-results] (save-find-result r))))

(defn matches-any-pattern? [s pp]
  (some #(re-find % s) pp))

(defn any-matches-any-pattern? [ss pp]
  (some #(not (= % nil)) (map #(matches-any-pattern? % pp) ss)))

(defn linesmatch? [lines inpatterns outpatterns]
  (and
    (or
      (empty? inpatterns)
      (any-matches-any-pattern? lines inpatterns))
    (or
      (empty? outpatterns)
      (not (any-matches-any-pattern? lines outpatterns)))))

(defn linesbefore-match? [linesbefore settings]
  (linesmatch? linesbefore (:in-linesbeforepatterns settings) (:out-linesbeforepatterns settings)))

(defn linesafter-match? [linesafter settings]
  (linesmatch? linesafter (:in-linesafterpatterns settings) (:out-linesafterpatterns settings)))

(defn get-newline-indices [s]
  (map first 
    (filter #(= (second %) \newline)
      (map-indexed vector s))))

(defn get-multiline-linesbefore [s beforestartindices beforeendindices settings]
  (if (> (:linesbefore settings) 0)
    (let [linesbefore (:linesbefore settings)
          startindices (take-last linesbefore beforestartindices)
          endindices (take-last linesbefore beforeendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn get-multiline-linesafter [s afterstartindices afterendindices settings]
  (if (> (:linesafter settings) 0)
    (let [linesafter (:linesafter settings)
          startindices (take linesafter afterstartindices)
          endindices (take linesafter afterendindices)]
      (if (and startindices endindices)
        (map #(.substring s (first %) (second %)) (map vector startindices endindices))
        []))
    []))

(defn find-multiline-string-for-pattern
  ([s p settings]
    (let [m (re-matcher p s)]
      (if (.find m 0)
        (let [newlineindices (get-newline-indices s)
              startlineindices (concat [0] (map inc newlineindices))
              endlineindices (concat newlineindices [(count s)])]
          (find-multiline-string-for-pattern s m 0 startlineindices
            endlineindices settings)))))
  ([s m i startlineindices endlineindices settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              beforestartindices (filter #(<= % startmatchindex) startlineindices)
              beforeendindices (filter #(< % startmatchindex) endlineindices)
              startlineindex (apply max beforestartindices)
              endlineindex (apply min (filter #(> % startmatchindex) endlineindices))
              line (.substring s startlineindex endlineindex)
              linenum (count beforestartindices)
              linesbefore (get-multiline-linesbefore s (butlast beforestartindices)
                beforeendindices settings)
              afterstartindices (filter #(> % startmatchindex) startlineindices)
              afterendindices (filter #(> % startmatchindex) endlineindices)
              linesafter (get-multiline-linesafter s afterstartindices
                (rest afterendindices) settings)
              result (->FindResult
                       (.pattern m)
                       nil 
                       linenum
                       (+ (- startmatchindex startlineindex) 1)
                       (+ (- endmatchindex startlineindex) 1)
                       line
                       linesbefore
                       linesafter)]
          (if
            (and
              (or
                (= (:linesbefore settings) 0)
                (linesbefore-match? linesbefore settings))
              (or
                (= (:linesafter settings) 0)
                (linesafter-match? linesafter settings)))
            (if (:firstmatch settings)
              [result]
              (concat [result] (find-multiline-string-for-pattern s m
                endmatchindex startlineindices endlineindices settings)))
            [])))
      [])))

(defn find-multiline-string [s settings]
  (apply concat
    (map #(find-multiline-string-for-pattern s % settings) (:findpatterns settings))))

(defn find-text-file-contents [sf settings]
  (let [contents (slurp (:file sf) :encoding (:textfileencoding settings))
        find-results (find-multiline-string contents settings)
        with-file-results (map #(assoc-in % [:file] sf) find-results)]
    (doseq [r with-file-results] (save-find-result r))))

(defn find-line-for-pattern
  ([linenum line linesbefore linesafter p settings]
    (let [m (re-matcher p line)]
      (if
        (and
          (.find m 0)
          (linesbefore-match? linesbefore settings)
          (linesafter-match? linesafter settings))
        (find-line-for-pattern linenum line linesbefore linesafter m 0 [] settings)
        [])))
  ([linenum line linesbefore linesafter m i results settings]
    (if (.find m i)
      (do
        (let [startmatchindex (.start m)
              endmatchindex (.end m)
              result (->FindResult
                       (.pattern m)
                       nil 
                       linenum
                       (+ startmatchindex 1)
                       (+ endmatchindex 1)
                       line
                       linesbefore
                       linesafter)]
          (find-line-for-pattern linenum line linesbefore linesafter m
            endmatchindex (concat results [result]) settings)))
      results)))

(defn find-line [linenum line linesbefore linesafter settings]
  (apply concat
    (map #(find-line-for-pattern linenum line linesbefore linesafter % settings)
      (:findpatterns settings))))

(defn find-lines
  ([lines settings]
    (let [line (first lines)
          nextlines (drop (:linesafter settings) (rest lines))
          linesbefore []
          linesafter (take (:linesafter settings) (rest lines))]
      (find-lines 1 line nextlines linesbefore linesafter [] settings)))
  ([linenum line lines linesbefore linesafter results settings]
    (if line
      (let [nextresults (find-line linenum line linesbefore linesafter settings)
            nextlinenum (+ linenum 1)
            nextline (if (empty? linesafter) (first lines) (first linesafter))
            nextlinesbefore
              (if (> (:linesbefore settings) 0)
                (if (= (count linesbefore) (:linesbefore settings))
                  (concat (rest linesbefore) [line])
                  (concat linesbefore [line]))
                [])
            nextlinesafter
              (if (> (:linesafter settings) 0)
                (concat (rest linesafter) (take 1 lines))
                [])]
          (find-lines nextlinenum nextline (rest lines) nextlinesbefore
            nextlinesafter (concat results nextresults) settings))
      (if
        (and
          (> (count results) 0)
          (:firstmatch settings))
        (take 1 results)
        results))))

(defn find-text-file-lines [sf settings]
  (with-open [rdr (reader (:file sf) :encoding (:textfileencoding settings))]
    (let [find-results (find-lines (line-seq rdr) settings)
          with-file-results (map #(assoc-in % [:file] sf) find-results)]
      (doseq [r with-file-results] (save-find-result r)))))

(defn find-text-file [sf settings]
  (if (:verbose settings)
    (log-msg (format "Finding text file %s" (find-file-path sf))))
  (if (:multilineoption-REMOVE settings)
    (find-text-file-contents sf settings)
    (find-text-file-lines sf settings)))

(defn find-file [sf settings]
  (let [filetype (:filetype sf)
        verbose (:verbose settings)
        filepath (find-file-path sf)]
    (cond
      (or
        (= filetype :code)
        (= filetype :text)
        (= filetype :xml)) (find-text-file sf settings)
      (= filetype :binary) (find-binary-file sf settings)
      (= filetype :archive)
        (if (:findarchives settings)
          (find-archive-file sf settings)
          (if verbose (log-msg (format "Skipping archive file %s" filepath))))
      :else
        (if verbose (log-msg (format "Skipping file of unknown type: %s" filepath))))))

(defn find-files [findfiles settings]
  (if (:verbose settings)
    (do
      (log-msg (format "\nFiles to be found (%d):" (count findfiles)))
      (doseq [sf findfiles] (log-msg (find-file-path sf)))
      (log-msg "")))
  (doseq [sf findfiles] (find-file sf settings)))

(defn find [settings]
  (let [errs (validate-settings settings)]
    (if (empty? errs)
      (let [startfile (file (:startpath settings))
            findfiles (if
                          (.isFile startfile) [(new-find-file startfile (get-filetype startfile))]
                          (get-find-files settings))]
        (find-files findfiles settings))
        [])
      errs))
