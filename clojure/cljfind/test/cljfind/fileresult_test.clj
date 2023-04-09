(ns cljfind.fileresult-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileresult :only (new-file-result file-result-path sort-results)]
        [cljfind.fileutil :only (get-name)]
        [cljfind.findsettings :only(DEFAULT-SETTINGS)]))

(deftest test-fileresult-abs-path
  (let [filepath "~/src/xfind/clojure/cljfind/src/cljfind/fileresult.clj"
        fileresult (new-file-result (file filepath) :code nil)]
    (testing "test-fileresult-abs-path"
             (is (= (file-result-path fileresult) filepath)))))

(deftest test-fileresult-rel-path-1
  (let [filepath "./fileresult.clj"
        fileresult (new-file-result (file filepath) :code nil)]
    (testing "test-fileresult-rel-path-1"
             (is (= (file-result-path fileresult) filepath)))))

(deftest test-fileresult-rel-path-2
  (let [filepath "../fileresult.clj"
        fileresult (new-file-result (file filepath) :code nil)]
    (testing "test-fileresult-rel-path-2"
             (is (= (file-result-path fileresult) filepath)))))

(deftest test-fileresult-sort-results-by-path
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        settings (assoc DEFAULT-SETTINGS :sort-by :filepath)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-path"
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "abc.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 3))))))))

(deftest test-fileresult-sort-results-by-path-case-insensitive
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        with-sort-by (assoc DEFAULT-SETTINGS :sort-by :filepath)
        settings (assoc with-sort-by :sort-caseinsensitive true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-path-case-insensitive"
             (is (= "abc.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 3))))))))

(deftest test-fileresult-sort-results-by-path-sort-descending
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        with-sort-by (assoc DEFAULT-SETTINGS :sort-by :filepath)
        settings (assoc with-sort-by :sort-descending true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-path-sort-descending"
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "abc.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 3))))))))

(deftest test-fileresult-sort-results-by-filename
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        settings (assoc DEFAULT-SETTINGS :sort-by :filename)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-filename"
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "abc.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 3))))))))

(deftest test-fileresult-sort-results-by-filename-case-insensitive
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        with-sort-by (assoc DEFAULT-SETTINGS :sort-by :filename)
        settings (assoc with-sort-by :sort-caseinsensitive true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-filename-case-insensitive"
             (is (= "abc.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 3))))))))

(deftest test-fileresult-sort-results-by-filename-sort-descending
  (let [results [(new-file-result (file "abc.txt") :text nil)
                 (new-file-result (file "xyz.txt") :text nil)
                 (new-file-result (file "PQR.txt") :text nil)
                 (new-file-result (file "hello/tuv.txt") :text nil)
                 ]
        with-sort-by (assoc DEFAULT-SETTINGS :sort-by :filename)
        settings (assoc with-sort-by :sort-descending true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-fileresult-sort-results-by-filename-sort-descending"
             (is (= "xyz.txt" (get-name (:file (nth sorted-results 0)))))
             (is (= "tuv.txt" (get-name (:file (nth sorted-results 1)))))
             (is (= "abc.txt" (get-name (:file (nth sorted-results 2)))))
             (is (= "PQR.txt" (get-name (:file (nth sorted-results 3))))))))
