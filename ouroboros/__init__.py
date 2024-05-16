# This Python script is primarily used for setting up and initializing a
# Lisp environment using the SBCL (Steel Bank Common Lisp)
# implementation. It does this by loading a precompiled Lisp core file,
# or generating a new one if necessary.

# The script starts by importing necessary modules. ctypes is used for
# creating and manipulating C data types in Python, os.path and pathlib
# for handling file paths, and subprocess for running commands in a new
# process.

# Next, it defines several paths using pathlib.Path. moduledir is the
# directory of the current script. gencore_path and core_path are paths
# to the Lisp files in the same directory. quicklisp_path is the path to
# the Quicklisp setup file, a library manager for Common Lisp.

# The script then checks if gencore_path and quicklisp_path exist using
# assert. If either file does not exist, the script will raise an
# AssertionError and stop.

# The script then checks if core_path exists or if it's older than
# gencore_path. If either condition is true, it uses subprocess.run to
# execute a series of SBCL commands that load the Quicklisp setup,
# define a parameter for the core path, and run the gencore.lisp script
# to regenerate the core file.

# The CDLL function is then used to load the SBCL shared library
# (libsbcl.so) into the Python process. This allows the script to call
# functions from the library.

# The initialize function is defined to initialize the Lisp
# environment. It takes a list of arguments, converts them to C-style
# strings (c_char_p), and passes them to the initialize_lisp function
# from the SBCL library.

# Finally, the initialize function is called with arguments to load the
# core file and suppress startup messages (--noinform).


from ctypes import CDLL, RTLD_GLOBAL, c_char_p
import os.path
import pathlib
import subprocess

moduledir = pathlib.Path(__file__).resolve().parent
gencore_path = pathlib.Path(moduledir, "gencore.lisp")
core_path = pathlib.Path(moduledir, "ouroboros.core")
quicklisp_path = pathlib.Path("~/quicklisp/setup.lisp").expanduser()

assert gencore_path.exists()
assert quicklisp_path.exists()

if ((not core_path.exists()) or
    # Ensure the core is newer than gencore.lisp
    (os.path.getmtime(core_path) < os.path.getmtime(gencore_path))):
    # (Re)generate the core file
    subprocess.run(
        ['sbcl',
         '--eval', f'(load "{quicklisp_path}")',
         '--eval', f'(defparameter cl-user::*ouroboros-core* "{core_path}")',
         '--script', str(gencore_path)])

libsbcl = CDLL("libsbcl.so", mode=RTLD_GLOBAL)

def initialize(args):
    """
    Initializes the Ouroboros library.

    Args:
        args (list): A list of command-line arguments.

    Returns:
        None
    """
    argc = len(args)
    argv = (c_char_p * argc)(*[c_char_p(arg.encode('utf-8')) for arg in args])
    libsbcl.initialize_lisp(argc, argv)

initialize(["", "--core", str(core_path), "--noinform"])
