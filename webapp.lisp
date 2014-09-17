(defpackage :bld-ssweb
  (:use :cl :hunchentoot :cl-who :parenscript :bld-ode :ht-simple-ajax :cl-fad)
  (:import-from :bld-utils make-hash lethash)
  (:import-from :cl-json encode-json decode-json-from-string *json-output*)
  (:import-from :cl-csv read-csv-row))

(in-package :bld-ssweb)

(defparameter *years* (* 2 pi))
(defparameter *tu* (/ (* 2 pi)))
(defparameter *deg* (/ pi 180))
(defparameter *rad* (/ 180 pi))

(setf (html-mode) :html5)

(defvar *sail-acceptor*
  (make-instance 
   'easy-acceptor :port 8080
   :document-root "/home/ben/src/lisp/bld-ssweb/www/"))
