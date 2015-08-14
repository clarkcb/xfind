;;; ############################################################################
;;;
;;; searchsettings.clj
;;;
;;; Defines the settings for a given search instance
;;;
;;; ############################################################################

(ns cljsearch.searchsettings
  #^{:author "Cary Clark",
     :doc "Defines the settings for a given search instance"}
  (:use [clojure.string :as str :only (split)]))

(defrecord SearchSettings
  [
    archivesonly
    debug
    excludehidden
    firstmatch
    in-archiveextensions
    in-archivefilepatterns
    in-dirpatterns
    in-extensions
    in-filepatterns
    in-linesafterpatterns
    in-linesbeforepatterns
    linesafter
    linesaftertopatterns
    linesafteruntilpatterns
    linesbefore
    listdirs
    listfiles
    listlines
    maxlinelength
    multilinesearch
    out-archiveextensions
    out-archivefilepatterns
    out-dirpatterns
    out-extensions
    out-filepatterns
    out-linesafterpatterns
    out-linesbeforepatterns
    printresults
    printusage
    printversion
    recursive
    searcharchives
    searchpatterns
    startpath
    uniquelines
    verbose
  ])

(def DEFAULT-SETTINGS (->SearchSettings
    false ; archivesonly
    false ; debug
    true  ; excludehidden
    false ; firstmatch
    #{}   ; in-archiveextensions
    #{}   ; in-archivefilepatterns
    #{}   ; in-dirpatterns
    #{}   ; in-extensions
    #{}   ; in-filepatterns
    #{}   ; in-linesafterpatterns
    #{}   ; in-linesbeforepatterns
    0     ; linesafter
    #{}   ; linesaftertopatterns
    #{}   ; linesafteruntilpatterns
    0     ; linesbefore
    false ; listdirs
    false ; listfiles
    false ; listlines
    150   ; maxlinelength
    false ; multilinesearch
    #{}   ; out-archiveextensions
    #{}   ; out-archivefilepatterns
    #{}   ; out-dirpatterns
    #{}   ; out-extensions
    #{}   ; out-filepatterns
    #{}   ; out-linesafterpatterns
    #{}   ; out-linesbeforepatterns
    true  ; printresults
    false ; printusage
    false ; printversion
    true  ; recursive
    false ; searcharchives
    #{}   ; searchpatterns
    nil   ; startpath
    false ; uniquelines
    false ; verbose
  ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extensions [settings exts extname]
  (if (empty? exts)
    settings
    (add-extensions
      (update-in settings [extname] #(add-element (first exts) %)) (rest exts) extname)))

(defn add-extension [settings ext extname]
  (add-extensions settings (str/split ext #",") extname))

(defn add-pattern [settings p patname]
  (update-in settings [patname] #(add-element (re-pattern p) %)))

(defn set-archivesonly [settings]
  (let [with-searcharchives (assoc settings :searcharchives true)]
    (assoc with-searcharchives :archivesonly true)))

(defn set-debug [settings]
  (let [with-verbose (assoc settings :verbose true)]
    (assoc with-verbose :debug true)))
