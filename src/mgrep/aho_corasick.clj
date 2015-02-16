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

(defrecord Trie [q delta final value])

(def trie-root 0)

(def trie-empty (Trie. #{trie-root} {} #{} {trie-root ""}))

(defn trie-add
  ([trie word]
    (trie-add trie trie-root word))
  ([trie cur-node word]
    (if (empty? word)
      (Trie. (:q trie) (:delta trie) (conj (:final trie) cur-node) (:value trie))
      (let [max-node (apply max (:q trie))
            succ-node (or (get-in (:delta trie) [cur-node (first word)])
                          (inc max-node))
            succ-value (str (get (:value trie) cur-node) (first word))
            new-q (conj (:q trie) succ-node)
            new-delta (assoc-in (:delta trie) [cur-node (first word)] succ-node)
            new-value (assoc (:value trie) succ-node succ-value)
            new-trie (Trie. new-q new-delta (:final trie) new-value)]
        (recur new-trie succ-node (next word))))))

(defn strings->trie [words]
  (reduce trie-add trie-empty words))

(defn trie-walk
  "Walks the trie. Returns either the final node or nil if path for given word doesn't exist."
  [trie word]
  (let [step (fn [node chr]
               (if (nil? node)
                 nil
                 (get-in (:delta trie) [node chr])))]
    (reduce step trie-root word)))

(defn trie-contains? [trie word]
  (let [node (trie-walk trie word)]
    (and (not (nil? node))
         (contains? (:final trie) node))))


;; actree is created by adding suffix links into a trie

; Aho-Corasick tree is also a trie
(defrecord ACTree [q deltap deltam final value])

(defn trie->actree [trie]
  (defn add-suffix-link [actree node]
    (let [node-value (get (:value trie) node)
          value-suffixes (suffixes node-value)
          suffix-node (or (take-first #(trie-contains? trie %) value-suffixes)
                          nil)
          new-deltam (assoc (:deltam actree) node suffix-node)]
    (ACTree. (:q actree) (:deltap actree) new-deltam (:final actree) (:value actree))))
  (let [initial-actree (ACTree. (:q trie) (:delta trie) {} (:final trie) (:value trie))]
    (reduce add-suffix-link initial-actree (:q trie))))

(defn actree-get-suffix-nodes [actree node]
  (if (nil? node)
    '()
    (cons node (actree-get-suffix-nodes actree (get (:deltam actree) node)))))

(defn actree-words-for-node [actree node]
  (let [suffix-nodes (actree-get-suffix-nodes actree node)
        dictionary-nodes (filter #(contains? (:final actree) %) suffix-nodes)
        values (map #((:value actree) %) dictionary-nodes)]
    values))

(defn actree-walk
  ([actree input]
    (actree-walk trie-root [] input))
  ([actree cur-node found input]
   (if (empty? input)
     found
     (let [succ-node (get-in (:deltap actree) [cur-node (first input)])
           suffix-node (get (:deltam actree) cur-node)
           consume-char? (or (not (nil? succ-node))
                             (nil? suffix-node))
           node-new (or succ-node suffix-node trie-root)
           found-new (into found ()) ; TODO
           input-new (if consume-char? (next input) input)]
       (recur actree node-new found-new input-new)))))

(let [act (trie->actree (strings->trie ["b" "ab"]))]
  (actree-get-suffix-nodes act 3))













