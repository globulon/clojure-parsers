(ns parsers.test.core
  (:use [parsers.core])
  (:use [clojure.test]))

(deftest zero-should-always-return-empty-element
  (is (= [] (zero "abc"))))

(deftest result-should-always-retain-information
  (is (= [["a" "bc"]] ((result "a") "bc"))))

(deftest item-with-non-empty-string-should-retain-first-elementt
  (is (= [[\a [\b \c]]] (item "abc"))))

(deftest item-with-empty-string-should-not-retain-info
  (is (= [] (item ""))))

(deftest chr-with-matching-character-should-retain-info
  (is (= [[\a [\b \c]]] ((chr \a) "abc"))))

(deftest chr-with-no-matching-character-should-return-empty
  (is (= [] ((chr \a) "bcd"))))

(deftest letter-with-first-letter-in-string-should-retain-info
  (is (= [[\a [\b \c]]] (letter "abc"))))

(deftest letter-with-no-first-letter-should-be-empty
  (is (= [] (letter "123"))))

(deftest digit-with-first-digi-in-string-should-retain-info
  (is (= [[\1 [\2 \3]]] (digit "123"))))

(deftest letter-with-no-first-digit-should-be-empty
  (is (= [] (digit "abc"))))

(deftest alphanum-with-first-digit-in-string-should-retain-info
  (is (= [[\1 [\a \b]]] (alphanum "1ab"))))

(deftest alphanum-with-first-letter-in-string-should-retain-info
  (is (= [[\a [\2 \3]]] (alphanum "a23"))))

(deftest alphanem-with-no-alphanum-should-be-empty
  (is (= [] (alphanum "***"))))

(deftest word-with-word-should-retain-info
  (is (=
        [[[\Y \e \s] [\!]] [[\Y \e] [\s \!]] [[\Y] [\e \s \!]] ["" "Yes!"]]
        (word "Yes!"))))