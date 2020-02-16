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
               ;; Consider these two being part of the same file?
               (:file "token-type")
               (:file "token")))
