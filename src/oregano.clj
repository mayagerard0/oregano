(ns oregano
  (:require [clojure.string :as str])
  (:import java.time.format.DateTimeFormatter))

(declare parse-element)

(defn parse-all-at-level [lines level]
  (loop [children []
         rem-lines lines]
    (if-let [[child r] (parse-element rem-lines level)]
      (recur (conj children child) r)
      [children rem-lines])))

(defn parse-element-body [body-lines]
  ;TODO Implement tables, lists, etc
  (when-not (empty? body-lines)
    (str/join "\n" body-lines)))


(defn parse-element [lines level]
  (when-let [[_ stars heading] (when-not (empty? lines)
                                 (re-matches #"(\*+)\s*(.*)" (first lines)))]
    (when (= (count stars) level)
      (let [[_ title-str tag-str] (re-matches #"(.*?)(?:\s:(.*):)?" heading)
            [_ status title-str] (re-matches #"\s*(READ|UNREAD|TODO|DONE)?(.*)" title-str)
            tags (if (some? tag-str) (set (str/split tag-str #":")) #{})
            [title url] (if-let [[_ url title] (re-matches #"\s*\[\[(.*)\]\[(.*)\]\]\s*" title-str)]
                          [title url]
                          [title-str nil])
            [body-lines body-rem] (split-with #(not= (first %) \*) (rest lines))
            [children rem] (parse-all-at-level body-rem (inc level))]
        [{:title (str/trim title)
          :status status
          :url   url
          :tags  tags
          :body  (parse-element-body body-lines)
          :children children}
         rem]))))

(defn print-element [{:keys [title status url tags body children]} level]
  (let [heading (str
                 (when (some? status) (str status " "))
                 (if (some? url)
                   (format "[[%s][%s]]" url title)
                   title)
                     (when-not (empty? tags)
                       (str " :" (str/join ":" tags) ":")))]
    (println (str (str/join (repeat level "*"))  " " heading))
    (when (some? body) (println body))
    (dorun (map #(print-element % (inc level)) children))))

(defn parse-org [raw-text]
 (let [all-lines (->> raw-text
                 str/split-lines
                 (filter #(> (count %) 0)))
       [title doc-lines]
       (if-let [[_ title] (re-matches #"#\+title: (.*)"  (first all-lines))]
         [title (rest all-lines)]
         [nil all-lines])
       [top-body elements-lines] (split-with #(not= (first %) \*) doc-lines)
      [elements _] (parse-all-at-level elements-lines 1)]
  {:title title
   :body (clojure.string/join "\n" top-body)
   :elements elements}))


(defn print-org [doc]
  (when-let [title (:title doc)]
    (println "#+title:" title))
  (when-let [body (:body doc)]
    (println body))
  (dorun (map #(print-element % 1) (:elements doc))))

(defn org-timestamp [d]
  (str "[" (.format d (DateTimeFormatter/ofPattern "yyyy-MM-dd E HH:mm")) "]"))

