;;;; clox.lisp

(in-package #:clox)

(defparameter *had-error-p* nil)

(defun prompt ()
  (loop (print "> ")
        (run (read-line))
        (setf *had-error-p* nil)))

(defun run (source)
  (let* ((scanner (make-scanner source))
         (tokens (scan-tokens scanner)))
    (dolist (token tokens)
      (format t "~a~%" token))))

(defun error% (line message)
  (report line "" message))

(defun report% (line where message)
  (format t "[line ~a] Error ~a: ~a~%" line where message)
  (setf *had-error-p* t))
