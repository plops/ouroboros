;; FILEPATH: ouroboros/ouroboros/low-level.lisp

;; This file contains low-level functions and macros for interacting with Python from Common Lisp.

(in-package #:ouroboros)

(defun call-with-python-error-handling (thunk)
  "Calls the supplied thunk function with Python error handling.
If an error occurs during the execution of the thunk, it will be caught and handled appropriately."
  (unwind-protect (funcall thunk)
    (pyerr-check-signals)
    (let ((err (pyerr-occurred)))
      (unless (cffi:null-pointer-p err)
        (pyerr-write-unraisable err)))))

(defmacro with-python-error-handling (&body body)
  "Executes the body forms with Python error handling.
Any errors that occur during the execution of the body forms will be caught and handled appropriately."
  `(call-with-python-error-handling (lambda () ,@body)))

(defun pyobject-typep (pyobject pytype)
  "Checks if the given PyObject is of the specified PyType."
  (pytype-subtypep (pyobject-pytype pyobject) pytype))

(defun pytuple (&rest pyobjects)
  "Creates a tuple PyObject from the supplied element PyObjects."
  (let* ((size (length pyobjects))
         (tuple (pytuple-new size)))
    (loop for position below size
          for pyobject in pyobjects
          do (pyobject-incref pyobject)
          do (pytuple-setitem tuple position pyobject))
    tuple))

(defun string-from-pyobject (pyobject)
  "Returns a Lisp string with the same content as the supplied PyObject.
If the PyObject is not a PyUnicode object, an error will be raised."
  (declare (pyobject pyobject))
  (unless (pyobject-typep pyobject *unicode-pyobject*)
    (error "Not a PyUnicode object: ~A." pyobject))
  (cffi:with-foreign-object (size-pointer :size)
    (let* ((char-pointer
             (with-python-error-handling
               (pyunicode-as-utf8-string pyobject size-pointer)))
           (nbytes (if (cffi:null-pointer-p char-pointer)
                       (error "Failed to convert string from Python to Lisp.")
                       (cffi:mem-ref size-pointer :size)))
           (octets (make-array nbytes :element-type '(unsigned-byte 8))))
      (loop for index below nbytes do
        (setf (aref octets index)
              (cffi:mem-aref char-pointer :uchar index)))
      (sb-ext:octets-to-string octets :external-format :utf-8))))

(defun pyobject-from-string (string)
  "Returns a PyObject with the same content as the supplied Lisp string."
  (declare (alexandria:string-designator string))
  (let* ((string (string string))
         (octets (sb-ext:string-to-octets string :external-format :utf-8))
         (nbytes (length octets)))
    (cffi:with-foreign-object (errors :pointer)
      (cffi:with-foreign-object (char-pointer :uchar nbytes)
        (loop for index below nbytes do
          (setf (cffi:mem-ref char-pointer :uchar index)
                (aref octets index)))
        (let ((pyobject
                (with-python-error-handling
                  (pyunicode-decode-utf8 char-pointer nbytes errors))))
          (when (cffi:null-pointer-p pyobject)
            (error "Failed to turn ~S into a PyUnicode object." string))
          (unless (pyobject-typep pyobject *unicode-pyobject*)
            (error "Not a PyUnicode object: ~S" pyobject))
          pyobject)))))

(defun pyprint (pyobject &optional (stream t))
  "Prints the string representation of the supplied PyObject to the specified stream."
  (let ((repr (with-python-error-handling (pyobject-repr pyobject))))
    (princ (string-from-pyobject repr) stream))
  pyobject)

;;; Calling Python Functions

(defun pycall (pycallable &rest pyobjects)
  "Calls the specified Python function with the given arguments.
The arguments should be PyObjects.
Returns the result of the function call as a PyObject."
  (let* ((argc (length pyobjects))
         (argv (make-array argc :element-type '(unsigned-byte 64))))
    (declare (dynamic-extent argv))
    (loop for position below argc for pyobject in pyobjects do
      (setf (aref argv position)
            (cffi:pointer-address pyobject)))
    (with-python-error-handling
      (pyobject-vectorcall pycallable (sb-sys:vector-sap argv) argc (cffi:null-pointer)))))

#+(or)
(define-compiler-macro pycall
    (&whole whole n-positional-arguments &rest arguments &environment environment)
  "A compiler macro for optimizing pycall.
Expands into a more efficient form for calling Python functions with positional and keyword arguments."
  (unless (integerp n-positional-arguments)
    (return-from pycall whole))
  (let* ((constantness
           (loop for argument in arguments
                 collect (constantp argument environment)))
         (argvalues
           (loop for argument in arguments
                 for constantp in constantness
                 collect
                 (if constantp argument (gensym))))
         (bindings
           (loop for argument in arguments
                 for argvalue in argvalues
                 for constantp in constantness
                 unless constantp
                   collect `(,argvalue ,argument)))
         (keyword-arguments
           (subseq argvalues (1+ n-positional-arguments)))
         (n-keyword-arguments (length keyword-arguments))
         (n-keywords
           (if (evenp n-keyword-arguments)
               (/ n-keyword-arguments 2)
               (progn (warn "Odd number of keyword arguments.")
                      (return-from pycall whole)))))
    `(let ,bindings
       (funcall (optimized-pycaller ,(+ n-positional-arguments n-keywords))
                ,@(subseq argvalues 0 (1+ n-positional-arguments))
                ,@(loop for (key value) on keyword-arguments by #'cddr collect value)
                (pycall-kwnames
                 ,@(loop for (key value) on keyword-arguments by #'cddr collect key))))))

#+(or)
(let ((pycallers (make-hash-table)))
  (defun optimized-pycaller (n-arguments)
    "A helper function for optimizing pycall.
Generates an optimized caller function for a given number of arguments."
    (values
     (alexandria:ensure-gethash
      n-arguments
      pycallers
      (compile
       nil
       (let ((arguments (loop for n below n-arguments collect (argument-symbol n))))
         `(lambda (function ,@arguments kwnames)
            (let ((argvector (make-array ,n-arguments :element-type '(unsigned-byte 64))))
              ,@(loop for position below n-arguments
                      for argument in arguments
                      collect
                      `(setf (aref argvector ,position)
                             (python-object-id ,argument)))
              (pyobject-vectorcall
               (python-from-lisp function)
               (sb-sys:vector-sap argvector)
               ,n-arguments
               kwnames)))))))))

#+(or)
(defun argument-symbol (n)
  "Generates a symbol for the nth argument in an optimized pycaller function."
  (intern (format nil "ARG~D" n) #.*package*))

#+(or)
(define-compiler-macro optimized-pycaller (&whole whole n-arguments)
  "A compiler macro for optimizing optimized-pycaller.
Expands into a load-time-value form to ensure that the optimized caller function is only compiled once."
  (if (constantp n-arguments)
      `(load-time-value
        (locally (declare (notinline optimized-pycaller))
          (optimized-pycaller ,n-arguments)))
      whole))
