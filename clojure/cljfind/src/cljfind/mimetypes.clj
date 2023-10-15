;;; ############################################################################
;;;
;;; mimetypes.clj
;;;
;;; Utility functions for getting MIME type
;;;
;;; ############################################################################

(ns cljfind.mimetypes
  #^{:author "Cary Clark",
     :doc "Module to provide mime-type-related utility functions"}
  (:import (java.io File))
  (:import (org.opf_labs LibmagicJnaWrapper))
  (:use [clojure.set :only (union)]
        [clojure.string :only (lower-case)]
        [cljfind.fileutil :only (get-ext get-name)]))

(defn get-libmagic
  "get the libmagic instance"
  []
  (let [flags (bit-or LibmagicJnaWrapper/MAGIC_MIME_TYPE
                      LibmagicJnaWrapper/MAGIC_NO_CHECK_ENCODING)
        magic-file-path "/usr/local/share/misc/magic.mgc"
        libmagic (doto (LibmagicJnaWrapper. flags) (.load magic-file-path))]
    libmagic))

(defn get-mime-type-getter
  "get a function to get the mime type of a file"
  [settings]
  (if
    (or
      (not (empty? (:in-mime-types settings)))
      (not (empty? (:out-mime-types settings))))
    (let [libmagic (get-libmagic)]
      (fn [^java.io.File f]
        (let [mime-type (.getMimeType libmagic (.getPath f))]
          (if (nil? mime-type) "" mime-type))))
      (fn [f] "")))
