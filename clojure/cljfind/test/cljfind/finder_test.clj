(ns cljfind.finder-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.config :only (SHAREDPATH)]
        [cljfind.fileutil :only (expand-path)]
        [cljfind.finder :only
          (filter-file? is-matching-archive-file? is-find-dir? is-matching-file?)]
        [cljfind.findsettings :only
          (DEFAULT-SETTINGS add-extension add-pattern set-archivesonly)]))

(def TESTFILE
  (str/join java.io.File/separator [SHAREDPATH "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-SETTINGS "Finder" :findpatterns))

;; *****************************************************************************
;; is-find-dir? tests
;; *****************************************************************************
(deftest test-is-find-dir?-default-settings
  (testing "test-is-find-dir?-default-settings"
    (is (is-find-dir? (file ".") DEFAULT-SETTINGS))
    (is (is-find-dir? (file "..") DEFAULT-SETTINGS))
    (is (not (is-find-dir? (file ".git") DEFAULT-SETTINGS)))
    (is (is-find-dir? (file "clojure") DEFAULT-SETTINGS))))

(deftest test-is-find-dir?-with-in-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :in-dirpatterns)]
    (testing "test-is-find-dir?-with-in-dirpatterns"
      (is (is-find-dir? (file "cljfind") settings))
      (is (is-find-dir? (file "finder") settings))
      (is (not (is-find-dir? (file "clojure") settings))))))

(deftest test-is-find-dir?-with-out-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "clojure" :out-dirpatterns)]
    (testing "test-is-find-dir?-with-out-dirpatterns"
      (is (is-find-dir? (file "cljfind") settings))
      (is (is-find-dir? (file "finder") settings))
      (is (not (is-find-dir? (file "clojure") settings))))))

(deftest test-is-find-dir?-with-include-hidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)]
    (testing "test-is-find-dir?-with-include-hidden"
    (is (is-find-dir? (file ".") settings))
    (is (is-find-dir? (file "..") settings))
    (is (is-find-dir? (file ".git") settings))
    (is (is-find-dir? (file "clojure") settings)))))

;; *****************************************************************************
;; is-file-result? tests
;; *****************************************************************************
(deftest test-is-file-result?-default-settings
  (testing "test-is-file-result?-default-settings"
    (is (is-matching-file? (file "finder.clj") DEFAULT-SETTINGS))))

(deftest test-is-file-result?-with-in-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)]
    (testing "test-is-file-result?-with-in-extensions"
      (is (is-matching-file? (file "finder.clj") settings))
      (is (is-matching-file? (file "finder.js") settings))
      (is (not (is-matching-file? (file "finder.py") settings))))))

(deftest test-is-file-result?-with-out-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "py" :out-extensions)]
    (testing "test-is-find-dir?-with-out-extensions"
      (is (is-matching-file? (file "finder.clj") settings))
      (is (is-matching-file? (file "finder.js") settings))
      (is (not (is-matching-file? (file "finder.py") settings))))))

(deftest test-is-file-result?-with-in-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :in-filepatterns)]
    (testing "test-is-file-result?-with-in-filepatterns"
      (is (is-matching-file? (file "cljfind.clj") settings))
      (is (is-matching-file? (file "finder.clj") settings))
      (is (not (is-matching-file? (file "fileutil.clj") settings))))))

(deftest test-is-file-result?-with-out-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :out-filepatterns)]
    (testing "test-is-find-dir?-with-out-filepatterns"
      (is (is-matching-file? (file "fileutil.clj") settings))
      (is (not (is-matching-file? (file "cljfind.clj") settings)))
      (is (not (is-matching-file? (file "finder.clj") settings))))))

;; *****************************************************************************
;; is-archive-file-result? tests
;; *****************************************************************************
(deftest test-is-archive-file-result?-default-settings
  (testing "test-is-archive-file-result?-default-settings"
    (is (is-matching-archive-file? (file "archive.zip") DEFAULT-SETTINGS))))

(deftest test-iis-archive-file-result?-with-in-earchivextensions
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)]
    (testing "test-is-archive-file-result?-with-in-archiveextensions"
      (is (is-matching-archive-file? (file "archive.zip") settings))
      (is (is-matching-archive-file? (file "archive.bz2") settings))
      (is (not (is-matching-archive-file? (file "archive.gz") settings))))))

(deftest test-is-archive-file-result?-with-out-archiveextensions
  (let [settings (add-extension DEFAULT-SETTINGS "gz" :out-archiveextensions)]
    (testing "test-is-archive-file-result?-with-out-archiveextensions"
      (is (is-matching-archive-file? (file "archive.zip") settings))
      (is (is-matching-archive-file? (file "archive.bz2") settings))
      (is (not (is-matching-archive-file? (file "archive.gz") settings))))))

(deftest test-is-archive-file-result?-with-in-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "arch" :in-archivefilepatterns)]
    (testing "test-is-archive-file-result?-with-in-archivefilepatterns"
      (is (is-matching-archive-file? (file "archive.zip") settings))
      (is (is-matching-archive-file? (file "arch.bz2") settings))
      (is (not (is-matching-archive-file? (file "compressed.gz") settings))))))

(deftest test-is-archive-file-result?-with-out-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "compress" :out-archivefilepatterns)]
    (testing "test-is-archive-file-result?-with-out-archivefilepatterns"
      (is (is-matching-archive-file? (file "archive.zip") settings))
      (is (is-matching-archive-file? (file "arch.bz2") settings))
      (is (not (is-matching-archive-file? (file "compressed.gz") settings))))))

;; *****************************************************************************
;; filter-file? tests
;; *****************************************************************************
(deftest test-filter-file?-default-settings
  (testing "test-filter-file?-default-settings"
    (is (filter-file? (file "finder.clj") DEFAULT-SETTINGS))))

(deftest test-filter-file?-with-file-result-settings
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)]
    (testing "test-filter-file?-with-file-result-settings"
      (is (filter-file? (file "finder.clj") settings))
      (is (filter-file? (file "finder.js") settings))
      (is (not (filter-file? (file "finder.py") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-archive-file-result-settings
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)]
    (testing "test-filter-file?-with-archive-file-result-settings"
      (is (not (filter-file? (file "archive.zip") settings)))
      (is (not (filter-file? (file "archive.bz2") settings)))
      (is (not (filter-file? (file "archive.gz") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-includearchives
  (let [settings (assoc DEFAULT-SETTINGS :includearchives true)]
    (testing "test-filter-file?-with-includearchives"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (filter-file? (file "finder.clj") settings))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-archivesonly
  (let [settings (set-archivesonly DEFAULT-SETTINGS true)]
    (testing "test-filter-file?-with-archivesonly"
      (is (filter-file? (file "archive.zip") settings))
      (is (filter-file? (file "archive.bz2") settings))
      (is (filter-file? (file "archive.gz") settings))
      (is (not (filter-file? (file "finder.clj") settings)))
      (is (not (filter-file? (file ".gitignore") settings))))))

(deftest test-filter-file?-with-includehidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)]
    (testing "test-filter-file?-with-includehidden"
      (is (filter-file? (file "finder.clj") settings))
      (is (filter-file? (file ".gitignore") settings))
      (is (not (filter-file? (file "archive.zip") settings))))))
