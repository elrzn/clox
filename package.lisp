;;;; package.lisp

(defpackage #:clox
  (:use #:cl)
  (:import-from :defclass-std :defclass/std)
  (:import-from :cl-algebraic-data-type :defdata))
