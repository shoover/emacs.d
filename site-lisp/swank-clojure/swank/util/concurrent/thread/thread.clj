(ns swank.util.concurrent.thread
  (:use (swank util)))

(defn- gen-name []
  (name (gensym "Thread-")))

(defn start-thread
  "Starts a thread that run the given function f"
  ([f]
     (doto (Thread. f)
       (start))))

(defmacro dothread [& body]
  `(start-thread (fn [] ~@body)))

(defmacro dothread-keeping [bindings & body]
  `(start-thread (keep-bindings ~bindings (fn [] ~@body))))

(defn current-thread []
  (Thread/currentThread))

(defn thread-set-name
  ([name] (thread-set-name (current-thread) name))
  ([#^Thread thread name]
     (.setName thread name)))

(defn thread-name
  ([] (thread-name (current-thread)))
  ([#^Thread thread]
     (.getName thread)))

(defn thread-id
  ([] (thread-id (current-thread)))
  ([#^Thread thread]
     (.getId thread)))

(defn thread-alive? [#^Thread t]
  (.isAlive t))
