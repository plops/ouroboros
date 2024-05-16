;; This file contains foreign function interface (FFI) definitions for interacting with the Python C API.
;; It provides CFFI definitions for various Python functions and types, allowing Lisp code to call Python functions and manipulate Python objects.

(in-package #:ouroboros)

;;; Py

(cffi:defcfun ("Py_Initialize" python-initialize) :void)
;; Initializes the Python interpreter. This function must be called before any other Python/C API functions are used.

(cffi:defcfun ("Py_InitializeEx" python-initialize-ex) :void
  (initsigs :bool))
;; Initializes the Python interpreter with additional options. The `initsigs` argument specifies whether to initialize the signal module.

(cffi:defcfun ("Py_IsInitialized" python-initializedp) :bool)
;; Checks if the Python interpreter has been initialized.

(cffi:defcfun ("Py_IncRef" pyobject-incref) :void
  (pyobject :pointer))
;; Increments the reference count of a Python object.

(cffi:defcfun ("Py_DecRef" pyobject-decref) :void
  (pyobject :pointer))
;; Decrements the reference count of a Python object.

(cffi:defcfun ("Py_ReprEnter" pyobject-repr-enter) :bool
  (pyobject :pointer))
;; Enters the object's representation. This is used to avoid infinite recursion when repr() is called on an object.

(cffi:defcfun ("Py_ReprLeave" pyobject-repr-leave) :void
  (pyobject :pointer))
;; Leaves the object's representation.

;;; PyObject

(cffi:defcfun ("PyObject_GetAttr" pyobject-getattr) :pointer
  (pyobject :pointer)
  (pystring :pointer))
;; Retrieves an attribute from a Python object using a string object as the attribute name.

(cffi:defcfun ("PyObject_HasAttr" pyobject-hasattr) :bool
  (pyobject :pointer)
  (pystring :pointer))
;; Checks if a Python object has a specific attribute using a string object as the attribute name.

(cffi:defcfun ("PyObject_GetAttrString" pyobject-getattr-string) :pointer
  (pyobject :pointer)
  (string :string))
;; Retrieves an attribute from a Python object using a string as the attribute name.

(cffi:defcfun ("PyObject_HasAttrString" pyobject-hasattr-string) :bool
  (pyobject :pointer)
  (string :string))
;; Checks if a Python object has a specific attribute using a string as the attribute name.

(cffi:defcfun ("PyObject_SetAttr" pyobject-setattr) :int
  (pyobject :pointer)
  (pystring :pointer)
  (pyvalue :pointer))
;; Sets an attribute on a Python object using a string object as the attribute name.

(cffi:defcfun ("PyObject_SetAttrString" pyobject-setattr-string) :int
  (pyobject :pointer)
  (string :string)
  (pyvalue :pointer))
;; Sets an attribute on a Python object using a string as the attribute name.

(cffi:defcfun ("PyObject_Repr" pyobject-repr) :pointer
  (pyobject :pointer))
;; Returns a string representation of a Python object.

(cffi:defcfun ("PyObject_Str" pyobject-str) :pointer
  (pyobject :pointer))
;; Returns a string representation of a Python object using the str() function.

(cffi:defcfun ("PyObject_ASCII" pyobject-ascii) :pointer
  (pyobject :pointer))
;; Returns a string representation of a Python object using the ascii() function.

(cffi:defcfun ("PyObject_Bytes" pyobject-bytes) :pointer
  (pyobject :pointer))
;; Returns a bytes representation of a Python object using the bytes() function.

(cffi:defcfun ("PyObject_IsTrue" pyobject-truep) :bool
  (pyobject :pointer))
;; Checks if a Python object is considered true.

(cffi:defcfun ("PyObject_Not" pyobject-not) :bool
  (pyobject :pointer))
;; Checks if a Python object is considered false.

(cffi:defcfun ("PyObject_Dir" pyobject-dir) :pointer
  (pyobject :pointer))
;; Returns a list of names in the namespace of a Python object.

(cffi:defcfun ("PyObject_GetIter" pyobject-iterator) :pointer
  (pyobject :pointer))
;; Returns an iterator object for a Python object.

(cffi:defcfun ("PyObject_GetAIter" pyobject-asynchronous-iterator) :pointer
  (pyobject :pointer))
;; Returns an asynchronous iterator object for a Python object.

;;; PyErr

(cffi:defcfun ("PyErr_Print" pyerr-print) :void)
;; Prints the current Python error to the standard error stream.

(cffi:defcfun ("PyErr_WriteUnraisable" pyerr-write-unraisable) :void
  (pyobject :pointer))
;; Writes an unraisable exception to the standard error stream.

(cffi:defcfun ("PyErr_SetNone" pyerr-set-none) :void
  (pyobject :pointer))
;; Sets the current Python error to None.

(cffi:defcfun ("PyErr_SetObject" pyerr-set-object) :void
  (pyobject :pointer)
  (pyvalue :pointer))
;; Sets the current Python error to a specific value.

(cffi:defcfun ("PyErr_Occurred" pyerr-occurred) :pointer)
;; Checks if an exception has occurred.

(cffi:defcfun ("PyErr_Clear" pyerr-clear) :void)
;; Clears the current Python error.

(cffi:defcfun ("PyErr_Fetch"  pyerr-fetch) :void
  (aptr :pointer)
  (bptr :pointer)
  (cptr :pointer))
;; Fetches the current Python error and clears it.

(cffi:defcfun ("PyErr_Restore"  pyerr-restore) :void
  (a :pointer)
  (b :pointer)
  (c :pointer))
;; Restores the previous Python error.

(cffi:defcfun ("PyErr_CheckSignals" pyerr-check-signals) :void)
;; Checks for signals and raises any pending exceptions.

;;; PyCallable

(cffi:defcfun ("PyCallable_Check" pycallablep) :bool
  (pyobject :pointer))
;; Checks if a Python object is callable.

(cffi:defcfun ("PyObject_CallNoArgs" pyobject-call-no-args) :pointer
  (pycallable :pointer))
;; Calls a callable Python object with no arguments.

(cffi:defcfun ("PyObject_CallOneArg" pyobject-call-one-arg) :pointer
  (pycallable :pointer)
  (arg :pointer))
;; Calls a callable Python object with one argument.

(cffi:defcfun ("PyObject_Call" pyobject-call) :pointer
  (pycallable :pointer)
  (args :pointer)
  (kwargs :pointer))
;; Calls a callable Python object with arguments and keyword arguments.

(cffi:defcfun ("PyObject_CallObject" pyobject-call-object) :pointer
  (pycallable :pointer)
  (args :pointer))
;; Calls a callable Python object with arguments.

(cffi:defcfun ("PyObject_Vectorcall" pyobject-vectorcall) :pointer
  (pycallable :pointer)
  (argvector :pointer)
  (nargsf :size)
  (kwnames :pointer))
;; Calls a callable Python object with a vector of arguments and keyword names.

;;; PyType

(cffi:defcfun ("PyType_GetName" pytype-name) :pointer
  (pytype :pointer))
;; Returns the name of a Python type.

(cffi:defcfun ("PyType_GetQualName" pytype-qualified-name) :pointer
  (pytype :pointer))
;; Returns the qualified name of a Python type.

(cffi:defcfun ("PyType_GetModuleName" pytype-module-name) :pointer
  (pytype :pointer))
;; Returns the name of the module that defines a Python type.

(cffi:defcfun ("PyType_IsSubtype" pytype-subtypep) :bool
  (pytype1 :pointer)
  (pytype2 :pointer))
;; Checks if one Python type is a subtype of another.

;;; PyLong

(cffi:defcfun ("PyLong_FromLong" pylong-from-long) :pointer
  (long :long))
;; Creates a Python long integer object from a C long.

(cffi:defcfun ("PyLong_FromDouble" pylong-from-double-float) :pointer
  (double :double))
;; Creates a Python long integer object from a C double.

;;; PyBool

(cffi:defcfun ("PyBool_FromLong" pybool-from-long) :pointer
  (long :long))
;; Creates a Python bool object from a C long.

;;; PyIter

(cffi:defcfun ("PyIter_Check" pyiterp) :bool
  (pyobject :pointer))
;; Checks if a Python object is an iterator.

(cffi:defcfun ("PyIter_Next" pyiter-next) :pointer
  (iterator :pointer))
;; Retrieves the next item from an iterator.

(cffi:defcfun ("PyIter_Send" pyiter-send) :int
  (iterator :pointer)
  (argument :pointer)
  (result* :pointer))
;; Sends a value to an iterator.

;;; PyNumber

(cffi:defcfun ("PyNumber_Check" pynumberp) :bool
  (pyobject :pointer))
;; Checks if a Python object is a number.

;;; PyUnicode

(cffi:defcfun ("PyUnicode_AsUTF8AndSize" pyunicode-as-utf8-string) :pointer
  (pyobject :pointer)
  (size-ptr (:pointer :size)))
;; Converts a Python Unicode object to a UTF-8 encoded C string.

(cffi:defcfun ("PyUnicode_DecodeUTF8" pyunicode-decode-utf8) :pointer
  (char-pointer :pointer)
  (size :size)
  (errors :pointer))
;; Decodes a UTF-8 encoded C string to a Python Unicode object.

;;; PyTuple

(cffi:defcfun ("PyTuple_New" pytuple-new) :pointer
  (size :size))
;; Creates a new Python tuple object with a specified size.

(cffi:defcfun ("PyTuple_Size" pytuple-size) :size
  (pytuple :pointer))
;; Returns the size of a Python tuple object.

(cffi:defcfun ("PyTuple_GetItem" pytuple-getitem) :pointer
  (pytuple :pointer)
  (position :size))
;; Retrieves an item from a Python tuple object at a specified position.

(cffi:defcfun ("PyTuple_SetItem" pytuple-setitem) :pointer
  (pytuple :pointer)
  (position :size)
  (pyvalue :pointer))
;; Sets an item in a Python tuple object at a specified position.

(cffi:defcfun ("PyTuple_Pack" pytuple-pack) :pointer
  (size :size) &rest)
;; Creates a new Python tuple object with a variable number of arguments.

;;; PyDict

(cffi:defcfun ("PyDict_New" pydict-new) :pointer)
;; Creates a new empty Python dictionary object.

(cffi:defcfun ("PyDict_GetItem" pydict-getitem) :pointer
  (pydict :pointer)
  (key :pointer))
;; Retrieves an item from a Python dictionary object using a key.

(cffi:defcfun ("PyDict_SetItem" pydict-setitem) :pointer
  (pydict :pointer)
  (key :pointer)
  (value :pointer))
;; Sets an item in a Python dictionary object using a key.

(cffi:defcfun ("PyDict_DelItem" pydict-delitem) :pointer
  (pydict :pointer)
  (key :pointer))
;; Deletes an item from a Python dictionary object using a key.

(cffi:defcfun ("PyDict_Clear" pydict-clear) :void
  (pydict :pointer))
;; Removes all items from a Python dictionary object.

(cffi:defcfun ("PyDict_Keys" pydict-keys) :pointer
  (pydict :pointer))
;; Returns a list of keys in a Python dictionary object.

(cffi:defcfun ("PyDict_Values" pydict-values) :pointer
  (pydict :pointer))
;; Returns a list of values in a Python dictionary object.

(cffi:defcfun ("PyDict_Values" pydict-items) :pointer
  (pydict :pointer))
;; Returns a list of key-value pairs in a Python dictionary object.

(cffi:defcfun ("PyDict_Size" pydict-size) :size
  (pydict :pointer))
;; Returns the number of items in a Python dictionary object.

(cffi:defcfun ("PyDict_Copy" pydict-copy) :pointer
  (pydict :pointer))
;; Creates a shallow copy of a Python dictionary object.

(cffi:defcfun ("PyDict_Contains" pydict-contains) :bool
  (pydict :pointer)
  (key :pointer))
;; Checks if a Python dictionary object contains a specific key.

;;; PyImport

(cffi:defcfun ("PyImport_GetModule" pyimport-getmodule) :pointer
  (pyobject :pointer))
;; Returns the module object for a given Python object.

;;; Initialization

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.12.so"
              "libpython3.11.so.1"
              "libpython3.11.so"))
  (t (:or (:default "libpython3.11")
          (:default "libpython3.11"))))

(unless (cffi:foreign-symbol-pointer "Py_IsInitialized")
  (cffi:use-foreign-library libpython))

(unless (python-initializedp)
  (python-initialize-ex nil))

;;; Pointers and Slots

(deftype pyobject ()
  "A raw reference to a Python object."
  'cffi:foreign-pointer)

(defun pyobject-address (pyobject)
  (cffi:pointer-address pyobject))

(declaim (pyobject *type-pyobject* *object-pyobject*))

(defparameter *type-pyobject*
  (cffi:foreign-symbol-pointer "PyType_Type"))

(defparameter *object-pyobject*
  (cffi:foreign-symbol-pointer "PyBaseObject_Type"))

(defparameter *none-pyobject*
  (cffi:foreign-symbol-pointer "_Py_NoneStruct"))

(defparameter *unicode-pyobject*
  (cffi:foreign-symbol-pointer "PyUnicode_Type"))

(declaim (type (integer 0 256) *pyobject-type-offset* *pyobject-refcount-offset*))

(defparameter *pyobject-type-offset*
  ;; We know that Python's type object is its own type, so we can determine the
  ;; offset by linear search.
  (loop for offset from 0 to 256 do
    (when (cffi:pointer-eq
           (cffi:mem-ref *type-pyobject* :pointer offset)
           *type-pyobject*)
      (return offset)))
  "The byte offset from the start of a Python object to the slot holding its type.")

(defun pyobject-pytype (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :pointer *pyobject-type-offset*))

(unless (cffi:pointer-eq (pyobject-pytype *object-pyobject*) *type-pyobject*)
  (error "Failed to determine the type offset of Python objects."))

(defparameter *pyobject-refcount-offset*
  (- *pyobject-type-offset* 8)
  "The byte offset from the start of a Python object to its reference count.")

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :size *pyobject-refcount-offset*))
