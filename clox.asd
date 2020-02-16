;;;; clox.asd

(asdf:defsystem #:clox
  :description "Describe clox here"
  :author "Eric Lorenzana"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:defclass-std
               #:cl-algebraic-data-type)
  :components ((:file "package")
               (:file "clox")
               (:file "token-type")))
