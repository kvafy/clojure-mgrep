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

