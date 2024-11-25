(ns cljs-http-missionary.core
  (:import [goog.net EventType XhrIo ErrorCode Jsonp])
  (:require [missionary.core :as m]
            [cljs-http-missionary.util :as util]))

(defn- aborted? [xhr]
  (= (.getLastErrorCode xhr) ErrorCode.ABORT))

(defn apply-default-headers!
  "Takes an XhrIo object and applies the default-headers to it."
  [xhr headers]
  (let [formatted-h (zipmap (map util/camelize (keys headers)) (vals headers))]
    (dorun
     (map (fn [[k v]]
            (.set (.-headers xhr) k v))
          formatted-h))))

(defn apply-response-type!
  "Takes an XhrIo object and sets response-type if not nil."
  [xhr response-type]
  (.setResponseType xhr
                    (case response-type
                      :array-buffer XhrIo.ResponseType.ARRAY_BUFFER
                      :blob XhrIo.ResponseType.BLOB
                      :document XhrIo.ResponseType.DOCUMENT
                      :text XhrIo.ResponseType.TEXT
                      :default XhrIo.ResponseType.DEFAULT
                      nil XhrIo.ResponseType.DEFAULT)))

(defn build-xhr
  "Builds an XhrIo object from the request parameters."
  [{:keys [with-credentials? default-headers response-type] :as request}]
  (let [timeout (or (:timeout request) 0)
        send-credentials (if (nil? with-credentials?)
                           true
                           with-credentials?)]
    (doto (XhrIo.)
      (apply-default-headers! default-headers)
      (apply-response-type! response-type)
      (.setTimeoutInterval timeout)
      (.setWithCredentials send-credentials))))

;; goog.net.ErrorCode constants to CLJS keywords
(def error-kw
  {0 :no-error
   1 :access-denied
   2 :file-not-found
   3 :ff-silent-error
   4 :custom-error
   5 :exception
   6 :http-error
   7 :abort
   8 :timeout
   9 :offline})

(def ^:private sentinel (js-obj))

(defn xhr
  "Execute the HTTP request corresponding to the given Ring request
  map and return a task.
  If *progress-flow atom provided in request, reset it by a progress-flow"
  [{:keys [request-method headers body *progress-flow] :as request}]
  (let [request-url (util/build-url request)
        method (name (or request-method :get))
        headers (util/build-headers headers)
        xhr (build-xhr request)
        *finished? (atom false)
        get-response-task
        (m/reduce
         (fn [_ v] (when-not (identical? sentinel v) (reduced v))) sentinel
         (m/relieve
          (m/observe
           (fn ctor [emit!]
             (.listen xhr EventType.COMPLETE
                      (fn [evt]
                        (let [target (.-target evt)
                              response {:status (.getStatus target)
                                        :success (.isSuccess target)
                                        :body (.getResponse target)
                                        :headers (util/parse-headers (.getAllResponseHeaders target))
                                        :trace-redirects [request-url (.getLastUri target)]
                                        :error-code (error-kw (.getLastErrorCode target))
                                        :error-text (.getLastError target)}]
                          (when-not (aborted? xhr) (emit! response)))))
             (.send xhr request-url method body headers)
             (fn dtor []
               (when-not (.isComplete xhr) (.abort xhr))
               (reset! *finished? true))))))]
    (when *progress-flow
      (reset! *progress-flow
              (m/stream
               (m/eduction
                (take-while #(not (identical? sentinel %)))
                (m/relieve
                 {}
                 (m/observe
                  (fn ctor [emit!]
                    (let [listener
                          (fn [direction evt]
                            (let [e (merge {:direction direction :loaded (.-loaded evt)}
                                           (when (.-lengthComputable evt) {:total (.-total evt)}))]
                              (emit! e)))]
                      (add-watch *finished? :end-flow
                                 (fn [_k _r _o n]
                                   (when (true? n) (emit! sentinel))))
                      (doto xhr
                        (.setProgressEventsEnabled true)
                        (.listen EventType.UPLOAD_PROGRESS (partial listener :upload))
                        (.listen EventType.DOWNLOAD_PROGRESS (partial listener :download))))
                    (fn []))))))))
    get-response-task))

(defn jsonp
  "Execute the JSONP request corresponding to the given Ring request
  map and return a task."
  [{:keys [timeout callback-name keywordize-keys?]
    :or {keywordize-keys? true}
    :as request}]
  (let [jsonp (Jsonp. (util/build-url request) callback-name)]
    (.setRequestTimeout jsonp timeout)
    (m/reduce
     (fn [_ v] (when-not (identical? sentinel v) (reduced v))) sentinel
     (m/relieve
      (m/observe
       (fn ctor [emit!]
         (.send jsonp nil
                (fn success-callback [data]
                  (let [response {:status 200
                                  :success true
                                  :body (js->clj data :keywordize-keys keywordize-keys?)}]
                    (emit! response)))
                (fn error-callback []
                  (emit! nil)))
         (fn [])))))))

(defn request
  "Execute the HTTP request corresponding to the given Ring request
  map and return a core.async channel."
  [{:keys [request-method] :as request}]
  (if (= request-method :jsonp)
    (jsonp request)
    (xhr request)))
