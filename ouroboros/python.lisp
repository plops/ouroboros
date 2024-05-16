;; This file defines a package that mimics the behavior of the Python programming
;; language, but using Lisp S-expressions as syntax.

(in-package #:ouroboros)

(defparameter python:|True| (mirror-into-lisp (pybool-from-long 1)))
(defparameter python:|False| (mirror-into-lisp (pybool-from-long 0)))

(defun truep (object)
  "Check if the given object is considered true in Python."
  (with-pyobjects ((pyobject object))
    (pyobject-truep pyobject)))

(defmacro python:|and| (&rest clauses)
  "Implement the 'and' logical operator in Python."
  `(if (and ,@(loop for clause in clauses collect `(truep ,clause)))
       python:|True|
       python:|False|))

(defmacro python:|assert| (expression)
  "Assert that the given expression evaluates to True in Python."
  (alexandria:with-gensyms (value)
    `(let ((,value ,expression))
       (unless (truep ,value)
         (error "Expression ~S evaluated to False."
                ',expression))
       ,value)))

(defmacro python:|await| (&rest rest)
  "Not yet implemented."
  (declare (ignore rest))
  (error "Not yet implemented."))

(defmacro python:|break| ()
  "Error: Encountered break statement outside of a loop."
  (error "Encountered break statement outside of a loop."))

(defmacro python:|case| (&body clauses)
  "Not yet implemented."
  (declare (ignore clauses))
  (error "Not yet implemented."))

(defmacro python:|class| (direct-superclasses &body body)
  "Not yet implemented."
  (declare (ignore direct-superclasses body))
  (error "Not yet implemented."))

(defmacro python:|continue| ()
  "Error: Encountered continue statement outside of a loop."
  (error "Encountered continue statement outside of a loop."))

(defmacro python:|def| (name lambda-list &body body)
  "Define a function in Python syntax."
  `(defun ,name ,lambda-list ,@body))

(defmacro python:|del| (variable)
  "Not yet implemented."
  (declare (ignore variable))
  (error "Not yet implemented."))

(defmacro python:|for| (variable iterable &body body)
  "Implement the 'for' loop in Python."
  (alexandria:with-gensyms (iterator nextp loop-start loop-end)
    `(let ((,iterator (make-iterator ,iterable)))
       (tagbody ,loop-start
          (multiple-value-bind (,variable ,nextp)
              (iterator-next ,iterator)
            (when (not ,nextp)
              (go ,loop-end))
            (macrolet ((python:|continue| ()
                         `(go ,',loop-start))
                       (python:|break| ()
                         `(go ,',loop-end)))
              ,@body))
          (go ,loop-start)
          ,loop-end)
       python:|None|)))

(defun make-iterator (iterable)
  "Create an iterator object for the given iterable."
  (with-pyobjects ((pyobject iterable))
    (mirror-into-lisp (pyobject-iterator pyobject))))

(defun iterator-next (iterator)
  "Get the next value from the iterator."
  (with-pyobjects ((pyiter iterator))
    (let* ((pynext (pyiter-next pyiter)))
      (if (cffi:null-pointer-p pynext)
          (values python:|None| nil)
          (values (mirror-into-lisp pynext) t)))))

(defmacro python:|if| (test then &optional (else python:|None|))
  "Implement the 'if' statement in Python."
  `(if (truep ,test)
       ,then
       ,else))

(defmacro python:|import| (module-name &optional (variable module-name))
  "Import a module in Python."
  `(defparameter ,variable (find-module ',module-name)))

(defmacro python:|import-from| (module-name &rest variables)
  "Import specific variables from a module in Python."
  (alexandria:with-gensyms (module)
    `(let ((,module (find-module ',module-name)))
       ,@(loop for variable in variables
               collect
               `(defparameter ,variable
                  (python:|getattr| ,module ',variable))))))

(defun find-module (module-name)
  "Find a module by its name."
  (let ((pymodulename (pyobject-from-string module-name)))
    (mirror-into-lisp
     (with-python-error-handling
       (prog1 (pyimport-getmodule pymodulename)
         (pyobject-decref pymodulename))))))

(defun python:|is| (&rest objects)
  "Check if all objects are the same in Python."
  (if (loop for (object . rest) on objects
            until (null rest)
            always (eq object (first rest)))
      python:|True|
      python:|False|))

(defmacro python:|lambda| (lambda-list &body body)
  "Define an anonymous function in Python syntax."
  `(lambda ,lambda-list ,@body))

(defmacro python:|match| (object &body patterns)
  "Not yet implemented."
  (declare (ignore object patterns))
  (error "Not yet implemented."))

(defun python:|not| (object)
  "Negate the truth value of the given object in Python."
  (with-pyobjects ((pyobject object))
    (if (pyobject-not pyobject)
        python:|True|
        python:|False|)))

(defmacro python:|or| (&rest clauses)
  "Implement the 'or' logical operator in Python."
  `(if (or ,@(loop for clause in clauses collect `(truep ,clause)))
       python:|True|
       python:|False|))

(defun python:|pass| ()
  "Do nothing in Python."
  python:|None|)

(defun (setf python:|getattr|) (value object attribute)
  "Set the value of an attribute in Python."
  (python:|setattr| object attribute value))

(defun python:|getattr| (object attribute)
  "Get the value of an attribute in Python."
  (let ((pyobject (mirror-into-python object))
        (pyattribute
          (pyobject-from-string attribute)))
    (unwind-protect (mirror-into-lisp (pyobject-getattr pyobject pyattribute))
      (pyobject-decref pyattribute))))

(defun python:|setattr| (object attribute value)
  "Set the value of an attribute in Python."
  (let ((pyobject (mirror-into-python object))
        (pyattribute
          (pyobject-from-string attribute))
        (pyvalue (mirror-into-python value)))
    (prog1 value
      (unwind-protect (pyobject-setattr pyobject pyattribute pyvalue)
        (pyobject-decref pyattribute)))))

(in-package #:python)

(named-readtables:in-readtable python:syntax)

(import-from
 builtins
 False
 True
 None
 NotImplemented
 Ellipsis
 __debug__
 abs
 aiter
 all
 anext
 any
 ascii
 bin
 bool
 breakpoint
 bytearray
 bytes
 callable
 chr
 classmethod
 compile
 complex
 delattr
 dict
 dir
 divmod
 enumerate
 eval
 exec
 filter
 float
 format
 frozenset
 getattr
 globals
 hasattr
 hash
 help
 hex
 id
 input
 int
 isinstance
 issubclass
 iter
 len
 list
 locals
 map
 max
 memoryview
 min
 next
 object
 oct
 open
 ord
 pow
 print
 property
 range
 repr
 reversed
 round
 set
 setattr
 slice
 sorted
 staticmethod
 str
 sum
 super
 tuple
 type
 vars
 zip
 __import__
 )
