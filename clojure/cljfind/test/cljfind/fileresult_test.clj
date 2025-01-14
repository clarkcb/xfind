(ns cljfind.fileresult-test
  (:use [clojure.java.io :only (file reader)])
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.fileresult :only (new-file-result file-result-path sort-results)]
        [cljfind.fileutil :only (get-path-name to-path)]
        [cljfind.findsettings :only (DEFAULT-FIND-SETTINGS)]))

(deftest test-file-result-abs-path
  (let [path "~/src/xfind/clojure/cljfind/src/cljfind/fileresult.clj"
        result (new-file-result (file path) :code 0 nil)]
    (testing "test-file-result-abs-path"
             (is (= (file-result-path result) path)))))

(deftest test-file-result-rel-path-1
  (let [path "./fileresult.clj"
        result (new-file-result (file path) :code 0 nil)]
    (testing "test-file-result-rel-path-1"
             (is (= (file-result-path result) path)))))

(deftest test-file-result-rel-path-2
  (let [path "../fileresult.clj"
        result (new-file-result (file path) :code 0 nil)]
    (testing "test-file-result-rel-path-2"
             (is (= (file-result-path result) path)))))

(deftest test-file-result-sort-results-by-path
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        settings (assoc DEFAULT-FIND-SETTINGS :sort-by :filepath)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-path"
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 3))))))))

(deftest test-file-result-sort-results-by-path-case-insensitive
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        with-sort-by (assoc DEFAULT-FIND-SETTINGS :sort-by :filepath)
        settings (assoc with-sort-by :sort-case-insensitive true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-path-case-insensitive"
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 3))))))))

(deftest test-file-result-sort-results-by-path-sort-descending
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        with-sort-by (assoc DEFAULT-FIND-SETTINGS :sort-by :filepath)
        settings (assoc with-sort-by :sort-descending true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-path-sort-descending"
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 3))))))))

(deftest test-file-result-sort-results-by-filename
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        settings (assoc DEFAULT-FIND-SETTINGS :sort-by :filename)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-filename"
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 3))))))))

(deftest test-file-result-sort-results-by-filename-case-insensitive
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        with-sort-by (assoc DEFAULT-FIND-SETTINGS :sort-by :filename)
        settings (assoc with-sort-by :sort-case-insensitive true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-filename-case-insensitive"
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 3))))))))

(deftest test-file-result-sort-results-by-filename-sort-descending
  (let [results [(new-file-result (to-path "abc.txt") :text 0 nil)
                 (new-file-result (to-path "xyz.txt") :text 0 nil)
                 (new-file-result (to-path "PQR.txt") :text 0 nil)
                 (new-file-result (to-path "hello/tuv.txt") :text 0 nil)
                 ]
        with-sort-by (assoc DEFAULT-FIND-SETTINGS :sort-by :filename)
        settings (assoc with-sort-by :sort-descending true)
        sorted-results (sort-results results settings)
        ]
    (testing "test-file-result-sort-results-by-filename-sort-descending"
             (is (= "xyz.txt" (get-path-name (:path (nth sorted-results 0)))))
             (is (= "tuv.txt" (get-path-name (:path (nth sorted-results 1)))))
             (is (= "abc.txt" (get-path-name (:path (nth sorted-results 2)))))
             (is (= "PQR.txt" (get-path-name (:path (nth sorted-results 3))))))))
