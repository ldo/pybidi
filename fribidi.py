#+
# Python-3 binding for Fribidy.
#-

import ctypes as ct

fribidi = ct.cdll.LoadLibrary("libfribidi.so.0")

class FRIBIDI :
    "useful definitions adapted from fribidi/*.h. You will need to use the constants," \
    " but apart from that, see the more Pythonic wrappers defined outside this" \
    " class in preference to accessing low-level structures directly."

    # General ctypes gotcha: when passing addresses of ctypes-constructed objects
    # to routine calls, do not construct the objects directly in the call. Otherwise
    # the refcount goes to 0 before the routine is actually entered, and the object
    # can get prematurely disposed. Always store the object reference into a local
    # variable, and pass the value of the variable instead.

    # FriBidiChar and FriBidiStrIndex are mapped to Python int

    INT8_LOCAL = ct.c_byte
    INT16_LOCAL = ct.c_short
    INT32_LOCAL = ct.c_int
    UINT8_LOCAL = ct.c_ubyte
    UINT16_LOCAL = ct.c_ushort
    UINT32_LOCAL = ct.c_uint
    BOOLEAN_LOCAL = ct.c_bool
    UNICHAR_LOCAL = ct.c_uint

    MAX_STRING_LENGTH = 0x7FFFFFFF

    UNICODE_CHARS = 0x110000

    UNICODE_VERSION = "6.2.0"
    UNICODE_MAJOR_VERSION = 6
    UNICODE_MINOR_VERSION = 2
    UNICODE_MICRO_VERSION = 0
    # more TBD

#end FRIBIDI

#+
# Routine arg/result types
#-

fribidi.fribidi_debug_status.restype = ct.c_int
fribidi.fribidi_debug_status.argtypes = ()
fribidi.fribidi_set_debug.restype = None
fribidi.fribidi_set_debug.argtypes = (ct.c_int,)

# more TBD

#+
# Higher-level stuff begins here
#-

def unicode_version() :
    "returns the supported Unicode version."
    return \
        ct.c_char_p.in_dll(fribidi, "fribidi_unicode_version").value.decode()
#end unicode_version
