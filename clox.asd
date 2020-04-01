;;;; clox.asd

(asdf:defsystem #:clox
  :description "A Clox interpreter."
  :author "Eric Lorenzana"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:defclass-std
               #:cl-algebraic-data-type
               #:trivial-types
               #:parse-number)
  :components ((:file "package")
               (:file "clox")
               (:file "token")
               (:file "scanner")))
