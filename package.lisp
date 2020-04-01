;;;; package.lisp

(defpackage #:clox
  (:use #:cl)
  (:import-from :cl-algebraic-data-type :defdata)
  (:import-from :parse-number :parse-number)
  (:import-from :alexandria :assoc-value))
