(ns cljfind.finder-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.config :only (XFINDPATH SHAREDPATH)]
        [cljfind.fileresult :only (new-file-result)]
        [cljfind.filetypes :only (get-file-type)]
        [cljfind.fileutil :only (to-path)]
        [cljfind.finder :only
         (filter-to-file-result find-files is-matching-archive-file-result? is-matching-dir? is-matching-file-result?)]
        [cljfind.findsettings :only
         (DEFAULT-FIND-SETTINGS add-extension add-path add-pattern set-archives-only)]))

(def TESTFILE
  (str/join java.io.File/separator [SHAREDPATH "testFiles" "testFile2.txt"]))

(defn get-settings []
  (add-pattern DEFAULT-FIND-SETTINGS "Finder" :findpatterns))

(def ^:const ^String BINPATH
  (str/join java.io.File/separator [XFINDPATH "bin"]))

;; *****************************************************************************
;; is-matching-dir? tests
;; *****************************************************************************
(deftest test-is-matching-dir?-default-settings
  (testing "test-is-matching-dir?-default-settings"
           (is (is-matching-dir? (to-path ".") DEFAULT-FIND-SETTINGS))
           (is (is-matching-dir? (to-path "..") DEFAULT-FIND-SETTINGS))
           (is (not (is-matching-dir? (to-path ".git") DEFAULT-FIND-SETTINGS)))
           (is (is-matching-dir? (to-path "clojure") DEFAULT-FIND-SETTINGS))))

(deftest test-is-matching-dir?-with-in-dir-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "find" :in-dir-patterns)]
    (testing "test-is-matching-dir?-with-in-dir-patterns"
             (is (is-matching-dir? (to-path "cljfind") settings))
             (is (is-matching-dir? (to-path "finder") settings))
             (is (not (is-matching-dir? (to-path "clojure") settings))))))

(deftest test-is-matching-dir?-with-out-dir-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "clojure" :out-dir-patterns)]
    (testing "test-is-matching-dir?-with-out-dir-patterns"
             (is (is-matching-dir? (to-path "cljfind") settings))
             (is (is-matching-dir? (to-path "finder") settings))
             (is (not (is-matching-dir? (to-path "clojure") settings))))))

(deftest test-is-matching-dir?-with-include-hidden
  (let [settings (assoc DEFAULT-FIND-SETTINGS :include-hidden true)]
    (testing "test-is-matching-dir?-with-include-hidden"
             (is (is-matching-dir? (to-path ".") settings))
             (is (is-matching-dir? (to-path "..") settings))
             (is (is-matching-dir? (to-path ".git") settings))
             (is (is-matching-dir? (to-path "clojure") settings)))))

;; *****************************************************************************
;; is-matching-file-result? tests
;; *****************************************************************************
(deftest test-is-matching-file-result?-default-settings
  (let [result (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code 0 nil)]
    (testing "test-is-matching-file-result?-default-settings"
             (is (is-matching-file-result? result DEFAULT-FIND-SETTINGS)))))

(deftest test-is-matching-file-result?-with-in-extensions
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "clj,js" :in-extensions)
        cljresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code 0 nil)
        jsresult (new-file-result (to-path "~/src/xfind/javascript/jsfind/src/finder.js") :code 0 nil)
        pyresult (new-file-result (to-path "~/src/xfind/python/pyfind/pyfind/finder.py") :code 0 nil)
        ]
    (testing "test-is-matching-file-result?-with-in-extensions"
      (is (is-matching-file-result? cljresult settings))
      (is (is-matching-file-result? jsresult settings))
      (is (not (is-matching-file-result? pyresult settings))))))

(deftest test-is-matching-file-result?-with-out-extensions
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "py" :out-extensions)
        cljresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code 0 nil)
        jsresult (new-file-result (to-path "~/src/xfind/javascript/jsfind/src/finder.js") :code 0 nil)
        pyresult (new-file-result (to-path "~/src/xfind/python/pyfind/pyfind/finder.py") :code 0 nil)
        ]
    (testing "test-is-matching-file-result?-with-out-extensions"
      (is (is-matching-file-result? cljresult settings))
      (is (is-matching-file-result? jsresult settings))
      (is (not (is-matching-file-result? pyresult settings))))))

(deftest test-is-matching-file-result?-with-in-file-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "find" :in-file-patterns)
        cljfindresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/cljfind.clj") :code 0 nil)
        finderresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code 0 nil)
        fileutilresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/fileutil.clj") :code 0 nil)
        ]
    (testing "test-is-matching-file-result?-with-in-file-patterns"
      (is (is-matching-file-result? cljfindresult settings))
      (is (is-matching-file-result? finderresult settings))
      (is (not (is-matching-file-result? fileutilresult settings))))))

(deftest test-is-matching-file-result?-with-out-file-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "find" :out-file-patterns)
        cljfindresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/cljfind.clj") :code 0 nil)
        finderresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/finder.clj") :code 0 nil)
        fileutilresult (new-file-result (to-path "~/src/xfind/clojure/cljfind/src/cljfind/fileutil.clj") :code 0 nil)
        ]
    (testing "test-is-matching-file-result?-with-out-file-patterns"
      (is (is-matching-file-result? fileutilresult settings))
      (is (not (is-matching-file-result? cljfindresult settings)))
      (is (not (is-matching-file-result? finderresult settings))))))

;; *****************************************************************************
;; is-matching-archive-file? tests
;; *****************************************************************************
(deftest test-is-matching-archive-file-result?-default-settings
  (let [result (new-file-result (to-path "./archive.zip") :archive 0 nil)]
    (testing "test-is-matching-archive-file-result?-default-settings"
             (is (is-matching-archive-file-result? result DEFAULT-FIND-SETTINGS)))))

(deftest test-is-matching-archive-file-result?-with-in-earchivextensions
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "zip,bz2" :in-archive-extensions)
        zipresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        bz2result (new-file-result (to-path "./archive.bz2") :archive 0 nil)
        gzresult (new-file-result (to-path "./archive.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-in-archive-extensions"
             (is (is-matching-archive-file-result? zipresult settings))
             (is (is-matching-archive-file-result? bz2result settings))
             (is (not (is-matching-archive-file-result? gzresult settings))))))

(deftest test-is-matching-archive-file-result?-with-out-archive-extensions
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "gz" :out-archive-extensions)
        zipresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        bz2result (new-file-result (to-path "./archive.bz2") :archive 0 nil)
        gzresult (new-file-result (to-path "./archive.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-out-archive-extensions"
             (is (is-matching-archive-file-result? zipresult settings))
             (is (is-matching-archive-file-result? bz2result settings))
             (is (not (is-matching-archive-file-result? gzresult settings))))))

(deftest test-is-matching-archive-file-result?-with-in-archive-file-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "arch" :in-archive-file-patterns)
        archiveresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        archresult (new-file-result (to-path "./arch.bz2") :archive 0 nil)
        compressedresult (new-file-result (to-path "./compressed.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-in-archive-file-patterns"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (not (is-matching-archive-file-result? compressedresult settings))))))

(deftest test-is-matching-archive-file-result?-with-out-archive-patterns
  (let [settings (add-pattern DEFAULT-FIND-SETTINGS "compress" :out-archive-file-patterns)
        archiveresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        archresult (new-file-result (to-path "./arch.bz2") :archive 0 nil)
        compressedresult (new-file-result (to-path "./compressed.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-out-archive-patterns"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (not (is-matching-archive-file-result? compressedresult settings))))))

(deftest test-is-matching-archive-file-result?-with-include-archives
  (let [settings (assoc DEFAULT-FIND-SETTINGS :include-archives true)
        archiveresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        archresult (new-file-result (to-path "./arch.bz2") :archive 0 nil)
        compressedresult (new-file-result (to-path "./compressed.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-include-archives"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (is-matching-archive-file-result? compressedresult settings)))))

(deftest test-is-matching-archive-file-result?-with-archives-only
  (let [settings (set-archives-only DEFAULT-FIND-SETTINGS true)
        archiveresult (new-file-result (to-path "./archive.zip") :archive 0 nil)
        archresult (new-file-result (to-path "./arch.bz2") :archive 0 nil)
        compressedresult (new-file-result (to-path "./compressed.gz") :archive 0 nil)
        ]
    (testing "test-is-matching-archive-file-result?-with-archives-only"
             (is (is-matching-archive-file-result? archiveresult settings))
             (is (is-matching-archive-file-result? archresult settings))
             (is (is-matching-archive-file-result? compressedresult settings)))))

;; *****************************************************************************
;; filter-to-file-result tests
;; *****************************************************************************
(deftest test-filter-to-file-result-default-settings
  (testing "test-filter-to-file-result-default-settings"
           (is (not (nil? (filter-to-file-result (to-path "finder.clj") DEFAULT-FIND-SETTINGS))))))

(deftest test-filter-to-file-result-with-file-result-settings
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "clj,js" :in-extensions)
        ]
    (testing "test-filter-to-file-result-with-file-result-settings"
      (is (not (nil? (filter-to-file-result (to-path "finder.clj") settings))))
      (is (not (nil? (filter-to-file-result (to-path "finder.js") settings))))
      (is (nil? (filter-to-file-result (to-path "finder.py") settings)))
      (is (nil? (filter-to-file-result (to-path ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-archive-file-result-settings
  (let [settings (add-extension DEFAULT-FIND-SETTINGS "zip,bz2" :in-archive-extensions)
        ]
    (testing "test-filter-to-file-result-with-archive-file-result-settings"
      (is (nil? (filter-to-file-result (to-path "archive.zip") settings)))
      (is (nil? (filter-to-file-result (to-path "archive.bz2") settings)))
      (is (nil? (filter-to-file-result (to-path "archive.gz") settings)))
      (is (nil? (filter-to-file-result (to-path ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-include-archives
  (let [settings (assoc DEFAULT-FIND-SETTINGS :include-archives true)
        ]
    (testing "test-filter-to-file-result-with-include-archives"
      (is (not (nil? (filter-to-file-result (to-path "archive.zip") settings))))
      (is (not (nil? (filter-to-file-result (to-path "archive.bz2") settings))))
      (is (not (nil? (filter-to-file-result (to-path "archive.gz") settings))))
      (is (not (nil? (filter-to-file-result (to-path "finder.clj") settings))))
      (is (nil? (filter-to-file-result (to-path ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-archives-only
  (let [settings (set-archives-only DEFAULT-FIND-SETTINGS true)
        zipfile (to-path "archive.zip")
        ziptype (get-file-type zipfile)
        zipresult (filter-to-file-result zipfile settings)
        bz2result (filter-to-file-result (to-path "archive.bz2") settings)
        gzresult (filter-to-file-result (to-path "archive.gz") settings)
        ]
    (testing "test-filter-to-file-result-with-archives-only"
      (is (not (nil? zipresult)))
      (is (not (nil? bz2result)))
      (is (not (nil? gzresult)))
      (is (nil? (filter-to-file-result (to-path "finder.clj") settings)))
      (is (nil? (filter-to-file-result (to-path ".gitignore") settings))))))

(deftest test-filter-to-file-result-with-includehidden
  (let [settings (assoc DEFAULT-FIND-SETTINGS :include-hidden true)
        ]
    (testing "test-filter-to-file-result-with-includehidden"
      (is (not (nil? (filter-to-file-result (to-path "finder.clj") settings))))
      (is (not (nil? (filter-to-file-result (to-path ".gitignore") settings))))
      (is (nil? (filter-to-file-result (to-path "archive.zip") settings))))))

;; *****************************************************************************
;; follow-symlink tests
;; *****************************************************************************
(deftest test-follow-symlink-default-settings
  (let [settings (add-path DEFAULT-FIND-SETTINGS (to-path BINPATH))
        [files errs] (find-files settings)
       ]
    (testing "test-follow-symlink-default-settings"
             (is (empty? errs))
             (is (< (count files) 3)))))

(deftest test-follow-symlink-with-followsymlinks
  (let [settings (assoc (add-path DEFAULT-FIND-SETTINGS (to-path BINPATH)) :follow-symlinks true)
        [files errs] (find-files settings)
       ]
    (testing "test-follow-symlink-with-followsymlinks"
             (is (empty? errs))
             (is (or (empty? files) (> (count files) 2))))))

(deftest test-follow-symlink-with-nofollowsymlinks
  (let [settings (assoc (add-path DEFAULT-FIND-SETTINGS (to-path BINPATH)) :follow-symlinks false)
        [files errs] (find-files settings)
       ]
    (testing "test-follow-symlink-with-nofollowsymlinks"
             (is (empty? errs))
             (is (< (count files) 3)))))
