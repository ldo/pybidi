#+
# Distutils script to install PyBidi. Invoke from the command line
# in this directory as follows:
#
#     python3 setup.py install
#
# Written by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#-

import distutils.core

distutils.core.setup \
  (
    name = "PyBidi",
    version = "1.0",
    description = "language bindings for FriBidi, for Python 3.3 or later",
    author = "Lawrence D'Oliveiro",
    author_email = "ldo@geek-central.gen.nz",
    url = "https://github.com/ldo/pybidi",
    py_modules = ["fribidi"],
  )
