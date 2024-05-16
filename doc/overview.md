## Ouroboros: Combining Python and Lisp

This project aims to bridge the gap between Python and Lisp (specifically, SBCL), allowing seamless interaction between both languages.

**Summary:**

* **Bidirectional interaction:** Ouroboros allows both calling Python code from Lisp and calling Lisp code from Python.
* **Mirroring objects:** Python objects are mirrored into Lisp as custom classes and instances, enabling manipulation using Lisp syntax. Similarly, Lisp objects can be mirrored into Python.
* **Python-like syntax in Lisp:** A dedicated package `python` allows using Python keywords and operators within Lisp S-expressions.

**Explanation:**

1. **Initialization:**
    * `ouroboros/__init__.py`:  Loads a pre-compiled Lisp core (`ouroboros.core`) generated from `ouroboros/gencore.lisp`.  This core embeds SBCL within the Python process.
    * `ouroboros/foreign.lisp`: Defines CFFI bindings for interacting with the Python C API.
    * `ouroboros/gencore.lisp`: Loads necessary Lisp libraries and saves the Lisp image as `ouroboros.core`.
2. **Mirroring into Lisp:**
    * `ouroboros/mirror-into-lisp.lisp`: Maps Python objects to Lisp counterparts using a hash table (`*python-object-table*`). 
    * Defines `python-object` and specialized subclasses for Python types like `type`, `object`, and `None`.
    * Provides functions for retrieving mirrored Lisp objects and converting Python strings to Lisp strings.
3. **Calling Python from Lisp:**
    * `ouroboros/low-level.lisp`: Defines functions for calling Python functions from Lisp, including handling Python errors.
    * Offers `pycall` macro for simplified Python function invocation using Lisp syntax.
4. **Mirroring into Python:**
    * `ouroboros/mirror-into-python.lisp`: Maps Lisp objects to Python counterparts using a hash table (`*lispobj-table*`).
    * Defines functions for mirroring Lisp classes and instances into Python objects.
5. **Python-like syntax in Lisp:**
    * `ouroboros/python.lisp`: Defines a package `python` that mimics Python syntax using Lisp S-expressions. 
    * Provides macros for Python keywords like `and`, `assert`, `for`, `if`, `import`, etc., along with operators and built-in functions.
    * `ouroboros/packages.lisp`: Defines the `python` package and exports its symbols.
6. **Readtables:**
    * `ouroboros/readtables.lisp`: Defines a custom readtable `python:syntax` for parsing Python-like syntax within Lisp.

**Not yet implemented:**

* Asynchronous operations using `async` and `await`.
* Python's `del` statement for variable deletion.
* `case` and `class` statements in Lisp mimicking Python syntax.

**Overall:**

Ouroboros presents a novel approach to combining Python and Lisp, offering bidirectional communication and a Python-like programming experience within a Lisp environment. However, certain Python features are not yet implemented.


