(ns mgrep.aho-corasick)


(defn suffixes [word]
  (for [i (range 1 (inc (count word)))]
    (subs word i)))

(defn take-first
  "Get first element of col for which given predicate is true. If none is found, return nil."
  [pred col]
  (cond (empty? col) nil
        (pred (first col)) (first col)
        :else (recur pred (next col))))


;; trie definition and construction

; Trie definition
;   qs    - set of states represented as strings
;   delta : qs x sigma -> qs
;         - transition function (partial function)
;   final - set of final states, subset of q
(defrecord Trie [qs delta final])

(def trie-root "")

(def trie-empty (Trie. #{trie-root} {} #{}))

(defn trie-add
  ([trie word]
    (trie-add trie trie-root word))
  ([trie q-cur word]
    (if (empty? word)
      (Trie. (:qs trie) (:delta trie) (conj (:final trie) q-cur))
      (let [q-succ (str q-cur (first word))
            qs-new (conj (:qs trie) q-succ)
            delta-new (assoc-in (:delta trie) [q-cur (first word)] q-succ)
            trie-new (Trie. qs-new delta-new (:final trie))]
        (recur trie-new q-succ (next word))))))

(defn strings->trie [words]
  (reduce trie-add trie-empty words))

(defn trie-walk
  "Walks the trie. Returns either the finishing state or nil if path for given word doesn't exist."
  [trie word]
  (let [step (fn [q chr]
               (if (nil? q)
                 nil
                 (get-in (:delta trie) [q chr])))]
    (reduce step trie-root word)))

(defn trie-contains? [trie word]
  (let [q (trie-walk trie word)]
    (and (not (nil? q))
         (contains? (:final trie) q))))


;; actree is created by adding suffix links into a trie

; TODO add deltad (dictionary transition function)

; Aho-Corasick tree is also a trie
;   qs     - set of states
;   deltap : qs x sigma -> qs
;          - positive transition function (partial function)
;   deltam : qs -> qs
;          - suffix-node transition function (partial function)
;   final  - set of final states
(defrecord ACTree [qs deltap deltam final])

(defn trie->actree [trie]
  (defn add-suffix-link [actree q]
    (let [q-suffix (or (take-first #(trie-contains? trie %) (suffixes q))
                       nil)
          deltam-new (assoc (:deltam actree) q q-suffix)]
    (ACTree. (:qs actree) (:deltap actree) deltam-new (:final actree))))
  (let [actree-initial (ACTree. (:qs trie) (:delta trie) {} (:final trie))]
    (reduce add-suffix-link actree-initial (:qs trie))))

(defn strings->actree [words]
  (trie->actree (strings->trie words)))

(defn actree-get-suffix-qs [actree q]
  (if (or (nil? q) (not (contains? (:qs actree) q)))
    '()
    (cons q (actree-get-suffix-qs actree (get (:deltam actree) q)))))

(defn actree-words-for-q [actree q]
  (let [suffix-qs (actree-get-suffix-qs actree q)
        dictionary-qs (filter #(contains? (:final actree) %) suffix-qs)]
    dictionary-qs))

(defn actree-walk
  ([actree input]
    (actree-walk actree trie-root [] input))
  ([actree q found input]
   (let [found-new (into found (actree-words-for-q actree q))]
     (if (empty? input)
       found-new
       (let [q-succ (get-in (:deltap actree) [q (first input)])
             q-suffix (get (:deltam actree) q)
             q-new (or q-succ q-suffix trie-root)
             consume-char? (or (not (nil? q-succ))
                               (= q q-new trie-root))
             input-new (if consume-char? (next input) input)]
         (recur actree q-new found-new input-new))))))
