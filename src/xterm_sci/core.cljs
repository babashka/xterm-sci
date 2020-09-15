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
(defonce ctx (atom nil))
(defonce initial-opts {:realize-max 1000
                       :preset :termination-safe
                       :classes {'js js/window}
                       :namespaces {'clojure.core {'*e last-error
                                                   'prn prn
                                                   'println println
                                                   'enable-safety!
                                                   (fn []
                                                     (reset! ctx (sci/init initial-opts))
                                                     nil)
                                                   'disable-safety!
                                                   (fn []
                                                     (.write term "Type (enable-safety!) to re-instate restrictions.")
                                                     (.write term "\r\n")
                                                     (reset! ctx (sci/init (dissoc initial-opts :preset :realize-max)))
                                                     nil)}}})

(reset! ctx (sci/init initial-opts))

(defn handle-error [last-error e]
  (sci/alter-var-root last-error (constantly e))
  (let [msg (ex-message e)]
    (.write term msg) (.write term "\r\n")
    (when (str/includes? msg "allow")
      (.write term "Type (disable-safety!) to drop restrictions.")
      (.write term "\r\n"))))

(defn read-form [line]
  (when-not (str/blank? line)
    (let [reader (sci/reader line)]
      (try
        (let [form (sci/with-bindings {sci/ns @last-ns}
                     (sci/parse-next @ctx reader))]
          (reset! last-form form))
        (catch :default e
          (cond (str/includes? (.-message e) "EOF while reading")
                ::eof-while-reading
                :else
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

(defn print-fn [sb v]
  (if (and (instance? goog.string/StringBuffer @sci/out)
           ;; we're in with-out-str
           (not (identical? sb @sci/out)))
    (.append @sci/out (str v "\n"))
    (.write term (str v "\r\n"))))

(defn eval! []
  (let [sb (goog.string/StringBuffer.)]
    (binding [*print-fn* (partial print-fn sb)]
      (sci/with-bindings {sci/ns @last-ns
                          sci/out sb ;; print and pr will print to sci/out
                          last-error @last-error}
        (when-not (= ::none @last-form)
          (let [ret (try
                      (sci/eval-form @ctx @last-form)
                      (catch :default e
                        (handle-error last-error e)
                        ::err)
                      (finally
                        (let [output (str sb)]
                          (when-not (str/blank? output)
                            (.write term
                                    (str/replace output #"\n$" "\r\n"))))))]
            (when-not (= ::err ret) ;; do nothing, continue in input-loop
              (print-val ret)
              (reset! last-ns @sci/ns))))))))

(defn prompt []
  (if @await-more-input "> "
      (str @last-ns "=> ")))

(defn input-loop []
  (.then (.read line-discipline (prompt))
         (fn [_]
           (eval!)
           (input-loop))))

(defonce i ;; don't start another input loop on hot-reload
  (do (print-fn nil "Welcome to xterm-sci.")
      (input-loop)))
