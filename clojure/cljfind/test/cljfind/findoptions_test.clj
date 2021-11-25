(ns cljfind.findoptions-test
  (:require [clojure.test :refer :all])
  (:use [clojure.string :as str :only (join)]
        [cljfind.findoptions :only (settings-from-args settings-from-json)]))

(deftest test-no-args
  (let [[ss errs] (settings-from-args [])]
    (testing "test-no-args"
      (is (not (:archivesonly ss)))
      (is (not (:debug ss)))
      (is (:excludehidden ss))
      (is (not (:listdirs ss)))
      (is (:listfiles ss))
      (is (not (:printusage ss)))
      (is (not (:printversion ss)))
      (is (:recursive ss))
      (is (not (:includearchives ss)))
      (is (empty? (:path ss)))
      (is (not (:verbose ss))))))

(deftest test-valid-args
  (let [[ss errs] (settings-from-args ["-x" "clj,scala" "."])]
    (testing "test-valid-args"
      (is (empty? errs))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "clj"))
      (is (contains? (:in-extensions ss) "scala"))
      (is (contains? (:paths ss) ".")))))

(deftest test-missing-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "." "-D"])]
    (testing "test-missing-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Missing arg for option D")))))

(deftest test-invalid-arg
  (let [[ss errs] (settings-from-args ["-x" "clj" "." "-Q"])]
    (testing "test-invalid-arg"
      (is (= (count errs) 1))
      (is (= (first errs) "Invalid option: Q")))))

(deftest test-archivesonly
  (let [[ss errs] (settings-from-args ["--archivesonly"])]
    (testing "test-archivesonly"
      (is (= (:archivesonly ss) true))
      (is (= (:includearchives ss) true)))))

(deftest test-debug
  (let [[ss errs] (settings-from-args ["--debug"])]
    (testing "test-debug"
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true)))))

(deftest test-settings-from-json
  (let [settings-json "{
  \"path\": \"~/src/xfind\",
  \"in-ext\": [\"js\",\"ts\"],
  \"out-dirpattern\": \"node_module\",
  \"out-filepattern\": [\"temp\"],
  \"debug\": true,
  \"includehidden\": true
}"
        [ss errs] (settings-from-json settings-json)
        ]
    (testing "test-debug"
      (is (= (count (:paths ss)) 1))
      (is (contains? (:paths ss) "~/src/xfind"))
      (is (= (count (:in-extensions ss)) 2))
      (is (contains? (:in-extensions ss) "js"))
      (is (contains? (:in-extensions ss) "ts"))
      (is (= (count (:out-dirpatterns ss)) 1))
      (is (= (count (:out-filepatterns ss)) 1))
      (is (= (:debug ss) true))
      (is (= (:verbose ss) true))
      (is (= (:excludehidden ss) false)))))
