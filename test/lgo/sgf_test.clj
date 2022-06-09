(ns lgo.sgf-test
  (:require [clojure.test :refer :all]
            [lgo.sgf :refer :all]))

(deftest filename-without-extension-test
  (testing "Testing cutting the extension from a filename."
    (is (= (filename-without-extension "test-file.2022.txt") "test-file.2022"))
    (is (= (filename-without-extension "hi") "hi"))))