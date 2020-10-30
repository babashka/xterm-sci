(ns xterm-sci.core
  (:require ["local-echo" :as local-echo :default local-echo-controller]
            ["xterm" :as xterm]
            [clojure.string :as str]
            [goog.string]
            [sci.core :as sci]))

(defonce term (doto (xterm/Terminal.)
                (.open (js/document.getElementById "app"))))

(defonce last-form (atom ::none))
(defonce line-counter (atom 0))
(defonce last-seen-position (atom [0 0 0]))
(defonce last-ns (atom @sci/ns))
(defonce last-error (sci/new-dynamic-var '*e nil))
(defonce ctx (atom nil))
(defonce initial-opts {:classes {'js js/window}
                       :namespaces {'clojure.core
                                    {'*e last-error
                                     'prn prn
                                     'println println}}})

(def ^:dynamic *debug* false)
(defn debug [& args]
  (when *debug*
    (.log js/console (apply str args))))

(reset! ctx (sci/init initial-opts))

(defn handle-error [last-error e]
  (sci/alter-var-root last-error (constantly e))
  (let [msg (ex-message e)]
    (.write term (str "\r\n" msg))))

(defn print-val [v]
  (debug "print-val" (str v))
  (binding [*print-length* 20]
    (let [printed (try
                    (pr-str v)
                    (catch :default e
                      (str "Error while printing: " (pr-str e))))]
      (.write term (str "\r\n" printed)))))

(defn print-fn [sb v]
  (if (and (instance? goog.string/StringBuffer @sci/out)
           ;; we're in with-out-str
           (not (identical? sb @sci/out)))
    (.append @sci/out (str v "\n"))
    (.write term (str "\r\n" v))))

(defn eval-form [form]
  (debug "eval!" (str form))
  (let [sb (goog.string/StringBuffer.)]
    (binding [*print-fn* (partial print-fn sb)]
      (sci/with-bindings {sci/ns @last-ns
                          sci/out sb ;; print and pr will print to sci/out
                          last-error @last-error}
        (when-not (= ::none form)
          (let [ret (try
                      (sci/eval-form @ctx form)
                      (catch :default e
                        (handle-error last-error e)
                        ::err)
                      (finally
                        (let [output (str sb)]
                          (when-not (str/blank? output)
                            (.write term
                                    (str "\r\n" output))))))]
            (when-not (= ::err ret) ;; do nothing, continue in input-loop
              (print-val ret)
              (reset! last-ns @sci/ns))
            ret))))))

(defn eval-forms [line]
  (when-not (str/blank? line)
    (let [reader (sci/reader line)]
      (loop []
        (let [ret
              (try
                (let [old-pos @last-seen-position
                      form (sci/with-bindings {sci/ns @last-ns}
                             (sci/parse-next @ctx reader))
                      new-pos [@line-counter
                               (sci/get-line-number reader)
                               (sci/get-column-number reader)]
                      new (pos? (compare new-pos old-pos))]
                  (when new (reset! last-seen-position new-pos))
                  (if (= ::sci/eof form)
                    form
                    (if new
                      (eval-form form)
                      (debug "skippig eval of form, already evaluated" (str form)))))
                (catch :default e
                  (cond (str/includes? (.-message e) "EOF while reading")
                        ::eof-while-reading
                        :else
                        (do
                          (handle-error last-error e)
                          ;;we're done handling this input
                          ::sci/eof))))]
          (if (#{::eof-while-reading ::sci/eof ::err} ret)
            ret
            (recur)))))))

(defonce line-discipline (local-echo-controller.
                          term #js
                          {:isIncompleteInput
                           (fn [line]
                             (let [v (eval-forms line)
                                   await-more (= ::eof-while-reading v)]
                               await-more))}))

(defn prompt []
  (swap! line-counter inc)
  (str @last-ns "=> "))

(defn input-loop []
  (.then (.read line-discipline (prompt))
         (fn [_]
           (input-loop))))

(defonce i ;; don't start another input loop on hot-reload
  (do (.write term "Welcome to xterm-sci.\r\n")
      (input-loop)))
