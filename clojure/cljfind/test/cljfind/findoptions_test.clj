(ns cljfind.findoptions-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.findoptions :only (settings-from-args settings-from-json)]))

(deftest test-no-args
  (let [[ss errs] (settings-from-args [])]
    (testing "test-no-args"
      (is (not (:archivesonly ss)))
      (is (:colorize ss))
      (is (not (:debug ss)))
      (is (:excludehidden ss))
      (is (not (:firstmatch ss)))
      (is (= (:linesafter ss) 0))
      (is (= (:linesbefore ss) 0))
      (is (not (:listdirs ss)))
      (is (not (:listfiles ss)))
      (is (not (:listlines ss)))
      (is (= (:maxlinelength ss) 150))
      (is (not (:multilineoption-REMOVE ss)))
      (is (:printresults ss))
      (is (not (:printusage ss)))
      (is (not (:printversion ss)))
      (is (:recursive ss))
      (is (not (:findarchives ss)))
      (is (= (:startpath ss) nil))
      (is (not (:uniquelines ss)))
      (is (not (:verbose ss))))))

(deftest test-valid-args
  (let [[ss errs] (settings-from-args ["-x" "clj,scala" "-s" "Find" "."])]
    (testing "test-valid-args"
      (is (empty? errs))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "clj"))
      (is (contains? (:in-extensions ss) "scala"))
      (is (= (count (:findpatterns ss)) 1))
      (is (= (.pattern (first (:findpatterns ss))) "Find"))
      (is (= (:startpath ss) ".")))))

(deftest test-missing-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Find" "." "-D"])]
    (testing "test-missing-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Missing arg for option D")))))

(deftest test-invalid-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "-s" "Find" "." "-Q"])]
    (testing "test-invalid-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Invalid option: Q")))))

(deftest test-archivesonly
  (let [[ss errs] (settings-from-args ["--archivesonly"])]
    (testing "test-archivesonly"
      (is (= (:archivesonly ss) true))
      (is (= (:findarchives ss) true)))))

(deftest test-debug
  (let [[ss errs] (settings-from-args ["--debug"])]
    (testing "test-debug"
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true)))))

(deftest test-settings-from-json
  (let [settings-json "{
  \"startpath\": \"~/src/xfind\",
  \"in-ext\": [\"js\",\"ts\"],
  \"out-dirpattern\": \"node_module\",
  \"out-filepattern\": [\"temp\"],
  \"findpattern\": \"Finder\",
  \"linesbefore\": 2,
  \"linesafter\": 2,
  \"debug\": true,
  \"allmatches\": false,
  \"includehidden\": true
}"
        [ss errs] (settings-from-json settings-json)
        ]
    (testing "test-debug"
      (is (= (:startpath ss) "~/src/xfind"))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "js"))
      (is (contains? (:in-extensions ss) "ts"))
      (is (= (count (:out-dirpatterns ss)) 1))
      (is (= (count (:out-filepatterns ss)) 1))
      (is (= (count (:findpatterns ss)) 1))
      (is (= (:linesbefore ss) 2))
      (is (= (:linesafter ss) 2))
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true))
      (is (= (:firstmatch ss) true))
      (is (= (:excludehidden ss) false)))))
