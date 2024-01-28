(ns cljfind.sortby-test
  (:require [clojure.test :refer :all])
  (:use [cljfind.findsettings :only (sort-by-from-name)]))

(deftest test-sort-by-from-name
  (testing "test-sort-by-from-name"
    (is (= (sort-by-from-name "FILENAME") :filename))
    (is (= (sort-by-from-name "filename") :filename))
    (is (= (sort-by-from-name "NAME") :filename))
    (is (= (sort-by-from-name "name") :filename))

    (is (= (sort-by-from-name "FILESIZE") :filesize))
    (is (= (sort-by-from-name "filesize") :filesize))
    (is (= (sort-by-from-name "SIZE") :filesize))
    (is (= (sort-by-from-name "size") :filesize))

    (is (= (sort-by-from-name "FILETYPE") :filetype))
    (is (= (sort-by-from-name "filetype") :filetype))
    (is (= (sort-by-from-name "TYPE") :filetype))
    (is (= (sort-by-from-name "type") :filetype))

    (is (= (sort-by-from-name "LASTMOD") :lastmod))
    (is (= (sort-by-from-name "lastmod") :lastmod))

    (is (= (sort-by-from-name "ANYTHING") :filepath))
    (is (= (sort-by-from-name "anything") :filepath))))
