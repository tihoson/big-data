(ns document-search.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn split-to-words
  [^String s]
  (-> s
      string/lower-case
      (string/split #"[^\u00BF-\u1FFF\u2C00-\uD7FF\w]+")))

;; посчитаем веса слов (возможно лучшим вариантом будет получать их из файла/бд)
(defn calc-words-weigths
  [^String file-content]
  (frequencies (split-to-words file-content)))

(defn is-file-exist
  [^String file-path]
  (.exists (io/as-file file-path)))

;; для чтения файла с весами
(defn read-docs-weights
  [^String file-path]
  (if (is-file-exist file-path)
    (json/read-str (slurp file-path))
    {}))

;; сбор информации о каждом документе
(defn get-doc-info
  [docs]
  (reduce (fn [m [k v]]
            (assoc m k
                   {:doc-weight v
                    :doc-length (count (slurp k))
                    :words-wight (calc-words-weigths (slurp k))}))
          {} docs))

;; подсчет веса запроса по документу с учетом веса документа
(defn calc-query-for-doc
  [doc-info query]
  (->> query
       split-to-words
       (into #{})
       (map (fn [word] 
              (get (:words-wight doc-info) word 0)))
       (map #(-> %
                 (* (:doc-weight doc-info))
                 (/ (:doc-length doc-info))
                 double))
       (reduce +)))

;; файл с весами документов / строка запроса / сколько документов в топе выводить
(defn -main
  [& args]
  (let [[file query str-limit] args
        limit (Integer/parseInt str-limit)]
    (->> file
         read-docs-weights
         get-doc-info
         (#(for [[k m] %] 
             [k (calc-query-for-doc m query)]))
         (sort-by second)
         reverse
         (take limit)
         (run! println))))