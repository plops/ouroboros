;;;; File: readtables.lisp
;;;; Purpose: This file defines a custom readtable named python:syntax for handling Python-like syntax.
;;;; The readtable is merged with the standard readtable and set to preserve case.
;;;; If enabled, a new read macro is defined for the character '['. When the Lisp reader encounters '[',
;;;; it reads a list of items delimited by ']', and converts that list into a Python list.

(in-package #:ouroboros)

(named-readtables:defreadtable python:syntax
  (:merge :standard)
  (:case :preserve)
  #+(or)
  (:macro-char #\[ #'(lambda (stream char)
                       (make-python-list
                        (read-delimited-list #\] stream)))))
