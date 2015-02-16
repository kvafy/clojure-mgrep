(ns mgrep.aho-corasick-test
  (:require [clojure.test :refer :all]
            [mgrep.aho-corasick :refer :all]))

(deftest trie-contains?-test
  (testing "trie-contains? for empty trie"
    (let [trie (strings->trie [])]
      (is (not (trie-contains? trie "")))
      (is (not (trie-contains? trie "a")))
      (is (not (trie-contains? trie "aa")))))
  (testing "trie-contains? for non-empty trie"
    (let [trie (strings->trie ["a" "b"])]
      (is (trie-contains? trie "a"))
      (is (trie-contains? trie "b"))
      (is (not (trie-contains? trie "c")))
      (is (not (trie-contains? trie "")))))
  (testing "trie-contains? empty string"
    (let [trie (strings->trie [""])]
      (is (trie-contains? trie ""))
      (is (not (trie-contains? trie "a")))))
  )


(deftest actree-walk-test
  (testing "actree for empty string"
    (let [actree (strings->actree [""])]
      (is (= (actree-walk actree "") [""]))
      (is (= (actree-walk actree "x") ["" ""]))))
  (testing "actree for single word"
    (let [actree (strings->actree ["a"])]
      (is (= (actree-walk actree "") []))
      (is (= (actree-walk actree "a") ["a"]))
      (is (= (actree-walk actree "aaa") ["a" "a" "a"]))))
  (testing "actree for multiple words with disjunct suffixes"
    (let [actree (strings->actree ["a" "b"])]
      (is (= (actree-walk actree "axaabyb") ["a" "a" "a" "b" "b"]))))
  (testing "actree for multiple words sharing with common suffixes"
    (let [actree (strings->actree ["a" "ab"])
          result (set (actree-walk actree "aabb"))]
      (is (contains? result "a"))
      (is (contains? result "ab"))))
  )

