(ns aoc2025.day02-test
  (:require [aoc2025.day02 :as sut]
            [clojure.test :refer [deftest is testing]]))

(deftest part-1
  (testing "lower bound for even length"
    (is (= 250 (sut/get-lower-bound "250244")))
    (is (= 251 (sut/get-lower-bound "250264")))
    (is (= 1 (sut/get-lower-bound "10")))
    (is (= 1 (sut/get-lower-bound "11"))))
  (testing "lower bound for odd length"
    (is (= 1 (sut/get-lower-bound "9")))
    (is (= 100 (sut/get-lower-bound "25026"))))
  (testing "upper bound for even length"
    (is (= 249 (sut/get-upper-bound "250244")))
    (is (= 250 (sut/get-upper-bound "250264")))
    (is (= 0 (sut/get-upper-bound "10")))
    (is (= 1 (sut/get-upper-bound "11")))
    (is (= 1 (sut/get-upper-bound "12"))))
  (testing "upper bound for odd length"
    (is (= 0 (sut/get-upper-bound "9")))
    (is (= 99 (sut/get-upper-bound "25026")))
    (is (= 999 (sut/get-upper-bound "2502648")))))
