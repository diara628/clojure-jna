; Copyright (c) Chris Houser, May 2009. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; Some extensions programmed by:
; Chris Nuernberger

(ns
  #^{:author "Chris Houser et al."
     :doc "Dynamically load and use native C libs from Clojure using JNA"}
  net.n01se.clojure-jna
  (:import (com.sun.jna Native Function)))

(defn- get-function [s]
  `(com.sun.jna.Function/getFunction ~(namespace s) ~(name s)))

(defmacro jna-invoke
  "Call a native library function:
  (jna-invoke Integer c/printf \"My number: %d\\n\" 5)"
  [return-type function-symbol & args]
  `(.invoke ~(get-function function-symbol) ~return-type (to-array [~@args])))

(defmacro jna-fn
  "Return a Clojure function that wraps a native library function:
   (def c-printf (jna-fn Integer c/printf))
   (c-printf \"My number: %d\\n\" 5)"
  [return-type function-symbol]
  `(let [func# ~(get-function function-symbol)]
     (fn [& args#]
       (.invoke func# ~return-type (to-array args#)))))

(defmacro jna-ns
  "Create a namespace full of Clojure functions that wrap functions from
  a native library:
  (jna-ns native-c c [Integer printf, Integer open, Integer close])
  (native-c/printf \"one %s two\\n\" \"hello\")"
  [new-ns libname fnspecs]
  `(do
     (create-ns '~new-ns)
     ~@(for [[return-type fn-name] (partition 2 fnspecs)]
         `(intern '~new-ns '~fn-name
                  (jna-fn ~return-type ~(symbol (str libname) (str fn-name)))))
     (the-ns '~new-ns)))

(defn make-cbuf
  "Create a direct ByteBuffer of the given size with little-endian
   byte order.  This is useful for creating structs to pass to
   native functions.  See also 'pointer'"
  [size]
  (-> (java.nio.ByteBuffer/allocateDirect size)
      (.order java.nio.ByteOrder/LITTLE_ENDIAN)))

(defn pointer
  "Pass in a ByteBuffer (such as created by make-cbuf) and this will
   return a JNA Pointer that can be passed directly to JNA-wrapped
   native functions."
  [direct-buffer]
  (when direct-buffer
    (Native/getDirectBufferPointer direct-buffer)))

(defn when-err
  "If value is negative one (-1), throws an excpetion with the given
   msg and the current errno.  Otherwise returns value."
  [value msg]
  (if (== -1 value)
    (throw (Exception. (str msg ", errno: " (Native/getLastError))))
    value))

(defn jna-malloc
  "Malloc a little endian byte buffer.  Returns
both the buffer and pointer with one call.
Found from a web-page:
http://nakkaya.com/2009/11/16/java-native-access-from-clojure/
-CN
"
  [byte-size] 
  (let [buffer (make-cbuf byte-size)
	pointer (pointer buffer)]
    {:pointer pointer :buffer buffer}))

(defn get-jna-fn
  "Get a function object from a given library.
libname - either short name, 'cudart' or long full pathname
fnname - name of the function
-CN"
  [lib-name fn-name]
  (Function/getFunction lib-name fn-name))

(defn jna-fn-to-fn
  "Given a jna function and a return type,
return a clojure function that will call the jna
function.
-CN"
  ([jna-fn rettype]
     (fn [& args]
       (.invoke jna-fn rettype (to-array args))))
  ([jna-fn]
     (fn [& args]
       (.invoke jna-fn (to-array args)))))

(defn create-jna-fn-def 
  "Given a function name, return type, and argument
datatypes, return a jna function definition.  Argument datatypes
are java primitive types, i.e. Float, Integer, Short, etc.
Pointers to those types are denoted by an array with :pointer
keywords for each level of indirection.  Thus you would have 

 (create-jna-fn 'addFloats' Float [:pointer Float] Integer)

which would create a jna function definition that summed an
array of floats of an integer length and returned the resulting
float.

Rettype may be nil in which case a void return value is assumed
-CN"
  [fn-name rettype & arg-types]
  (let [retval {:name fn-name
		:arguments arg-types}]
    (if rettype
      (assoc retval :return-type rettype)
      retval)))

(defn create-namespaced-jna-fn-def
  "See create-jna-fn-def.
Additional argument is a sequence of string namespace
names.  These are not clojure namespaces but c++
namespace names you have in the code.
 (def fdef (create-namespaced-jna-fn-def 
            [\"test1\" \"test2\"] 
           \"add_floatNumber\" 
            Float 
            Float Float))
-CN
"
  [namespaces fn-name rettype & arg-types]
  (let [retval (apply create-jna-fn-def fn-name rettype arg-types)]
    (assoc retval :namespaces namespaces)))

(defn- maybe-to-seq [arg]
  (try
   (seq arg)
   (catch Exception e
     nil)))

(def typename-map
     { Float "f"
      Integer "i"
      :pointer "P" })

(defn get-cpp-mangled-name 
  "Return the c++ mangled name for a given
function definition.  Please help fill out the
typename map so we support more types.

Result with namespaces:
input:
 {:namespaces [\"test1\" \"test2\"], :return-type java.lang.Float, :name \"add_floatNumber\", :arguments ([:pointer java.lang.Float] java.lang.Integer)}

output:
_ZN5test15test215addFloatNumbersEPfi


Result without namespaces:
input:
 {:return-type java.lang.Float, :name \"add_floatNumber\", :arguments (java.lang.Float java.lang.Float)}

output:
_Z15add_floatNumberff
-CN
"
  [jna-fn-def]
  (let [namelen (count (:name jna-fn-def))
	builder (StringBuilder.)
	has_namespaces (not (nil? (:namespaces jna-fn-def)))]
    (.append builder "_Z")
    (when has_namespaces
      (.append builder "N"))
    (doseq [namespace (:namespaces jna-fn-def)]
      (.append builder (.length namespace))
      (.append builder namespace))
    (.append builder namelen)
    (.append builder (:name jna-fn-def))
    (when has_namespaces
      (.append builder "E"))
    (if (seq? (:arguments jna-fn-def))
      (doseq [arg (:arguments jna-fn-def)]
	(if (maybe-to-seq arg)
	  (doseq [part arg]
	    (.append builder (typename-map part)))
	  (.append builder (typename-map arg))))
      (.append builder "v"))
    (.toString builder)))
