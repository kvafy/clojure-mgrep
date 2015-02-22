(ns mgrep.cmdline
  (:gen-class))

(require '[clojure.tools.cli])
(require '[mgrep.aho-corasick :as ac])


(defn lazy-reader
  "Takes a reader and returns the characters as a lazy sequence."
  [reader]
  (let [c (.read reader)]
    (if (= c -1)
      '()
      (lazy-cat (Character/toChars c) (lazy-seq (lazy-reader reader))))))


(defn search-for-patterns-in-file [actree patterns filename]
  (try
    (with-open [reader (clojure.java.io/reader filename)]
      (doseq [match (ac/actree-walk actree (lazy-reader reader))]
        (printf "%s: %s\n" filename match)))
    (catch java.io.IOException e
      (binding [*out* *err*]
          (printf "Error reading file '%s': %s.\n" filename (.toString e))))))

(defn search-for-patterns [patterns filenames]
  (let [actree (ac/strings->actree patterns)]
    (doseq [filename filenames]
      (search-for-patterns-in-file actree patterns filename))))


;; main functions and the command line interface

(defn usage [options-summary]
  (clojure.string/join \newline
                       [""
                        "Aim of the mgrep utility is to search for a big number of pattern/keywords"
                        "within a large number of files at once using algorithm with O(n) time"
                        "complexity where n is the length of the all input files."
                        ""
                        "Usage:"
                        options-summary
                        ""]))

(def cli-options
  [["-p" "--pattern-file PATTERN_FILE" "File containing patterns to search for. One pattern per line."]
   ["-h" "--help" "Print this help."
    :default false]])

(defn -main [& argv]
  (do
    (let [{:keys [options errors summary arguments]} (clojure.tools.cli/parse-opts argv cli-options)
          help (get options :help)]
      (cond help
              (println (usage summary))
            errors
              (do (println "Following errors encountered while processing the parameters:")
                  (println (clojure.string/join "\n" (map #(str "* " %) errors))))
            :else
              (try
                (let [patterns-file (get options :pattern-file)
                      patterns (clojure.string/split-lines (slurp patterns-file))]
                  (search-for-patterns patterns arguments))
                (catch java.io.IOException e
                  (binding [*out* *err*]
                      (printf "Encountered the following error: %s." (.toString e)))))))
    (.flush *out*)
    (.flush *err*)))
