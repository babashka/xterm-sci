(ns xterm-sci.core
  (:require ["local-echo" :as local-echo :default local-echo-controller]
            ["xterm" :as xterm]
            [clojure.string :as str]
            [goog.string]
            [sci.core :as sci]))

(defonce term (doto (xterm/Terminal.)
                (.open (js/document.getElementById "app"))))

(defonce last-form (atom ::none))
(defonce await-more-input (atom false))
(defonce last-ns (atom @sci/ns))
(defonce last-error (sci/new-dynamic-var '*e nil))
(defonce ctx (sci/init {:realize-max 1000
                        :profile :termination-safe
                        :classes {'js js/window}
                        :namespaces {'clojure.core {'*e last-error}}}))

(defn handle-error [last-error e]
  (sci/alter-var-root last-error (constantly e))
  (.write term (ex-message e))
  (.write term "\r\n"))

(defn read-form [line]
  (when-not (str/blank? line)
    (let [reader (sci/reader line)]
      (try
        (let [form (sci/parse-next ctx reader)]
          (reset! last-form form))
        (catch :default e
          (if (str/includes? (.-message e) "EOF while reading")
            ::eof-while-reading
            (do
              (handle-error last-error e)
              ;;we're done handling this input
              ::sci/eof)))))))

(defonce line-discipline (local-echo-controller.
                          term #js
                          {:isIncompleteInput
                           (fn [line]
                             (= ::eof-while-reading (read-form line)))}))

(defn print-val [v]
  (binding [*print-length* 20]
    (let [printed (try
                    (pr-str v)
                    (catch :default e
                      (str "Error while printing: " (pr-str e))))]
      (.write term (str printed "\r\n")))))

(defn eval! []
  (sci/with-bindings {sci/ns @last-ns
                      last-error @last-error
                      sci/out (goog.string/StringBuffer.)}
    (when-not (= ::none @last-form)
      (let [ret (try
                  (sci/eval-form ctx @last-form)
                  (catch :default e
                    (handle-error last-error e)
                    ::err)
                  (finally
                    (let [output (str @sci/out)]
                      (when-not (str/blank? output)
                        (.write term
                                (str/replace output #"\n$" "\r\n"))))))]
        (when-not (= ::err ret) ;; do nothing, continue in input-loop
          (print-val ret)
          (reset! last-ns @sci/ns))))))

(defn prompt []
  (if @await-more-input "> "
      (str @last-ns "=> ")))

(defn input-loop []
  (.then (.read line-discipline (prompt))
         (fn [_]
           (eval!)
           (input-loop))))

(defonce i ;; don't start another input loop on hot-reload
  (input-loop))