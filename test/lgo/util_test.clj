(ns lgo.util-test
  (:require [clojure.test :refer :all]
            [lgo.util :refer :all]))

(deftest vec-rm-test
  (testing "Testing 'removing' an element from a vector."
    (is (= (vec-rm [1 2 3] 1) [1 3]))
    (is (= (vec-rm [1 2 3] 2) [1 2]))
    (is (= (vec-rm [1 2 3] 0) [2 3]))))

(deftest vec-rm-all-test
  (testing "Testing 'removing' an element from a vector."
    (is (= (vec-rm-all [1 2 3] [1 2]) [1]))
    (is (= (vec-rm-all [1 2 3] [2 0]) [2]))
    (is (= (vec-rm-all [1 2 3] [0]) [2 3]))))

(deftest filename-without-extension-test
  (testing "Testing cutting the extension from a filename."
    (is (= (filename-without-extension "test-file.2022.txt") "test-file.2022"))
    (is (= (filename-without-extension "hi") "hi"))))
