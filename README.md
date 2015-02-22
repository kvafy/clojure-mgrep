# Overview
Mgrep is a Clojure utility designed to search for large ammounts of literal patterns inside given files. It uses Aho-Corasick algorithm to achieve O(n) time complexity in the length of the input, number of searched patterns is not relevant.


# Usage
`$ lein uberjar`
`$ java -jar target/mgrep-<version>-standalone.jar -p <pattern-file> <input-files>`


# Requirements
* Clojure 1.6 or compatible
