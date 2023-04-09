(ns cljfind.finder-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.config :only (SHAREDPATH)]
        [cljfind.fileresult :only (new-file-result)]
        [cljfind.filetypes :only (get-filetype)]
        [cljfind.fileutil :only (expand-path hidden-file?)]
        [cljfind.finder :only
         (filter-to-file-result is-matching-archive-file-result? is-matching-dir? is-matching-file-result?)]
        [cljfind.findsettings :only
          (DEFAULT-SETTINGS add-extension add-pattern set-archivesonly)]))

(def TESTFILE
  (str/join java.io.File/separator [SHAREDPATH "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-SETTINGS "Finder" :findpatterns))

;; *****************************************************************************
;; is-matching-dir? tests
;; *****************************************************************************
(deftest test-is-matching-dir?-default-settings
  (testing "test-is-matching-dir?-default-settings"
           (is (is-matching-dir? (file ".") DEFAULT-SETTINGS))
           (is (is-matching-dir? (file "..") DEFAULT-SETTINGS))
           (is (not (is-matching-dir? (file ".git") DEFAULT-SETTINGS)))
           (is (is-matching-dir? (file "clojure") DEFAULT-SETTINGS))))

(deftest test-is-matching-dir?-with-in-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :in-dirpatterns)]
    (testing "test-is-matching-dir?-with-in-dirpatterns"
             (is (is-matching-dir? (file "cljfind") settings))
             (is (is-matching-dir? (file "finder") settings))
             (is (not (is-matching-dir? (file "clojure") settings))))))

(deftest test-is-matching-dir?-with-out-dirpatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "clojure" :out-dirpatterns)]
    (testing "test-is-matching-dir?-with-out-dirpatterns"
             (is (is-matching-dir? (file "cljfind") settings))
             (is (is-matching-dir? (file "finder") settings))
             (is (not (is-matching-dir? (file "clojure") settings))))))

(deftest test-is-matching-dir?-with-include-hidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)]
    (testing "test-is-matching-dir?-with-include-hidden"
             (is (is-matching-dir? (file ".") settings))
             (is (is-matching-dir? (file "..") settings))
             (is (is-matching-dir? (file ".git") settings))
             (is (is-matching-dir? (file "clojure") settings)))))

;; *****************************************************************************
;; is-matching-file? tests
;; *****************************************************************************
(deftest test-is-matching-file-result?-default-settings
  (let [fileresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code nil)]
    (testing "test-is-matching-file-result?-default-settings"
      (is (is-matching-file-result? fileresult DEFAULT-SETTINGS)))))

(deftest test-is-matching-file-result?-with-in-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)
        cljresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code nil)
        jsresult (new-file-result (file "~/src/xfind/javascript/jsfind/src/finder.js") :code nil)
        pyresult (new-file-result (file "~/src/xfind/python/pyfind/pyfind/finder.py") :code nil)
        ]
    (testing "test-is-matching-file-result?-with-in-extensions"
      (is (is-matching-file-result? cljresult settings))
      (is (is-matching-file-result? jsresult settings))
      (is (not (is-matching-file-result? pyresult settings))))))

(deftest test-is-matching-file-result?-with-out-extensions
  (let [settings (add-extension DEFAULT-SETTINGS "py" :out-extensions)
        cljresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code nil)
        jsresult (new-file-result (file "~/src/xfind/javascript/jsfind/src/finder.js") :code nil)
        pyresult (new-file-result (file "~/src/xfind/python/pyfind/pyfind/finder.py") :code nil)
        ]
    (testing "test-is-matching-file-result?-with-out-extensions"
      (is (is-matching-file-result? cljresult settings))
      (is (is-matching-file-result? jsresult settings))
      (is (not (is-matching-file-result? pyresult settings))))))

(deftest test-is-matching-file-result?-with-in-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :in-filepatterns)
        cljfindresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/cljfind.clj") :code nil)
        finderresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code nil)
        fileutilresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/fileutil.clj") :code nil)
        ]
    (testing "test-is-matching-file-result?-with-in-filepatterns"
      (is (is-matching-file-result? cljfindresult settings))
      (is (is-matching-file-result? finderresult settings))
      (is (not (is-matching-file-result? fileutilresult settings))))))

(deftest test-is-matching-file-result?-with-out-filepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "find" :out-filepatterns)
        cljfindresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/cljfind.clj") :code nil)
        finderresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code nil)
        fileutilresult (new-file-result (file "~/src/xfind/clojure/cljfind/src/cljfind/fileutil.clj") :code nil)
        ]
    (testing "test-is-matching-file-result?-with-out-filepatterns"
      (is (is-matching-file-result? fileutilresult settings))
      (is (not (is-matching-file-result? cljfindresult settings)))
      (is (not (is-matching-file-result? finderresult settings))))))

;; *****************************************************************************
;; is-matching-archive-file? tests
;; *****************************************************************************
(deftest test-is-matching-archive-file-result?-default-settings
  (let [fileresult (new-file-result (file "./archive.zip") :archive nil)]
    (testing "test-is-matching-archive-file-result?-default-settings"
             (is (is-matching-archive-file-result? fileresult DEFAULT-SETTINGS)))))

(deftest test-is-matching-archive-file-result?-with-in-earchivextensions
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)
        zipresult (new-file-result (file "./archive.zip") :archive nil)
        bz2result (new-file-result (file "./archive.bz2") :archive nil)
        gzresult (new-file-result (file "./archive.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-in-archiveextensions"
             (is (is-matching-archive-file-result? zipresult settings))
             (is (is-matching-archive-file-result? bz2result settings))
             (is (not (is-matching-archive-file-result? gzresult settings))))))

(deftest test-is-matching-archive-file-result?-with-out-archiveextensions
  (let [settings (add-extension DEFAULT-SETTINGS "gz" :out-archiveextensions)
        zipresult (new-file-result (file "./archive.zip") :archive nil)
        bz2result (new-file-result (file "./archive.bz2") :archive nil)
        gzresult (new-file-result (file "./archive.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-out-archiveextensions"
             (is (is-matching-archive-file-result? zipresult settings))
             (is (is-matching-archive-file-result? bz2result settings))
             (is (not (is-matching-archive-file-result? gzresult settings))))))

(deftest test-is-matching-archive-file-result?-with-in-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "arch" :in-archivefilepatterns)
        archiveresult (new-file-result (file "./archive.zip") :archive nil)
        archresult (new-file-result (file "./arch.bz2") :archive nil)
        compressedresult (new-file-result (file "./compressed.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-in-archivefilepatterns"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (not (is-matching-archive-file-result? compressedresult settings))))))

(deftest test-is-matching-archive-file-result?-with-out-archivefilepatterns
  (let [settings (add-pattern DEFAULT-SETTINGS "compress" :out-archivefilepatterns)
        archiveresult (new-file-result (file "./archive.zip") :archive nil)
        archresult (new-file-result (file "./arch.bz2") :archive nil)
        compressedresult (new-file-result (file "./compressed.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-out-archivefilepatterns"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (not (is-matching-archive-file-result? compressedresult settings))))))

(deftest test-is-matching-archive-file-result?-with-includearchives
  (let [settings (assoc DEFAULT-SETTINGS :includearchives true)
        archiveresult (new-file-result (file "./archive.zip") :archive nil)
        archresult (new-file-result (file "./arch.bz2") :archive nil)
        compressedresult (new-file-result (file "./compressed.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-includearchives"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (is-matching-archive-file-result? compressedresult settings)))))

(deftest test-is-matching-archive-file-result?-with-archivesonly
  (let [settings (set-archivesonly DEFAULT-SETTINGS true)
        archiveresult (new-file-result (file "./archive.zip") :archive nil)
        archresult (new-file-result (file "./arch.bz2") :archive nil)
        compressedresult (new-file-result (file "./compressed.gz") :archive nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-archivesonly"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (is-matching-archive-file-result? compressedresult settings)))))

;; *****************************************************************************
;; filter-file? tests
;; *****************************************************************************
(deftest test-filter-to-file-result-default-settings
  (testing "test-filter-to-file-result-default-settings"
    (is (not (nil? (filter-to-file-result (file "finder.clj") DEFAULT-SETTINGS))))))

(deftest test-filter-to-file-result-with-file-result-settings
  (let [settings (add-extension DEFAULT-SETTINGS "clj,js" :in-extensions)
        ]
    (testing "test-filter-to-file-result-with-file-result-settings"
      (is (not (nil? (filter-to-file-result (file "finder.clj") settings))))
      (is (not (nil? (filter-to-file-result (file "finder.js") settings))))
      (is (nil? (filter-to-file-result (file "finder.py") settings)))
      (is (nil? (filter-to-file-result (file ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-archive-file-result-settings
  (let [settings (add-extension DEFAULT-SETTINGS "zip,bz2" :in-archiveextensions)
        ]
    (testing "test-filter-to-file-result-with-archive-file-result-settings"
      (is (nil? (filter-to-file-result (file "archive.zip") settings)))
      (is (nil? (filter-to-file-result (file "archive.bz2") settings)))
      (is (nil? (filter-to-file-result (file "archive.gz") settings)))
      (is (nil? (filter-to-file-result (file ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-includearchives
  (let [settings (assoc DEFAULT-SETTINGS :includearchives true)
        ]
    (testing "test-filter-to-file-result-with-includearchives"
      (is (not (nil? (filter-to-file-result (file "archive.zip") settings))))
      (is (not (nil? (filter-to-file-result (file "archive.bz2") settings))))
      (is (not (nil? (filter-to-file-result (file "archive.gz") settings))))
      (is (not (nil? (filter-to-file-result (file "finder.clj") settings))))
      (is (nil? (filter-to-file-result (file ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-archivesonly
  (let [settings (set-archivesonly DEFAULT-SETTINGS true)
        zipfile (file "archive.zip")
        ziptype (get-filetype zipfile)
        zipresult (filter-to-file-result zipfile settings)
        bz2result (filter-to-file-result (file "archive.bz2") settings)
        gzresult (filter-to-file-result (file "archive.gz") settings)
        ]
    (testing "test-filter-to-file-result-with-archivesonly"
      (is (not (nil? zipresult)))
      (is (not (nil? bz2result)))
      (is (not (nil? gzresult)))
      (is (nil? (filter-to-file-result (file "finder.clj") settings)))
      (is (nil? (filter-to-file-result (file ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-includehidden
  (let [settings (assoc DEFAULT-SETTINGS :excludehidden false)
        ]
    (testing "test-filter-to-file-result-with-includehidden"
      (is (not (nil? (filter-to-file-result (file "finder.clj") settings))))
      (is (not (nil? (filter-to-file-result (file ".gitignore") settings))))
      (is (nil? (filter-to-file-result (file "archive.zip") settings))))))
