#+
# Python-3 binding for Fribidi.
#
# Authors:
#   Lawrence D'Oliveiro, 2016
# C original by
#   Behdad Esfahbod, 2001, 2002, 2004
#
# Copyright © 2016 Lawrence D'Oliveiro.
# Copyright (C) 2004 Sharif FarsiWeb, Inc.
# Copyright (C) 2001,2002 Behdad Esfahbod
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library, in a file named COPYING; if not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA
#-

import ctypes as ct

fribidi = ct.cdll.LoadLibrary("libfribidi.so.0")

class FRIBIDI :
    "useful definitions adapted from fribidi/*.h. You will need to use the constants" \
    " and “macro” functions, but apart from that, see the more Pythonic wrappers" \
    " defined outside this class in preference to accessing low-level structures directly."

    # General ctypes gotcha: when passing addresses of ctypes-constructed objects
    # to routine calls, do not construct the objects directly in the call. Otherwise
    # the refcount goes to 0 before the routine is actually entered, and the object
    # can get prematurely disposed. Always store the object reference into a local
    # variable, and pass the value of the variable instead.

    # do I need these?
    INT8_LOCAL = ct.c_byte
    INT16_LOCAL = ct.c_short
    INT32_LOCAL = ct.c_int
    UINT8_LOCAL = ct.c_ubyte
    UINT16_LOCAL = ct.c_ushort
    UINT32_LOCAL = ct.c_uint
    BOOLEAN_LOCAL = ct.c_bool
    UNICHAR_LOCAL = ct.c_uint

    Level = ct.c_byte
    # FriBidiChar, FriBidiStrIndex, FriBidiCharType and FriBidiParType all mapped to Python int
    Char = ct.c_uint
    StrIndex = ct.c_int # unsigned?
    CharType = ct.c_uint
    ParType = ct.c_uint

    MAX_STRING_LENGTH = 0x7FFFFFFF

    UNICODE_CHARS = 0x110000

    UNICODE_VERSION = "6.2.0"
    UNICODE_MAJOR_VERSION = 6
    UNICODE_MINOR_VERSION = 2
    UNICODE_MICRO_VERSION = 0

    # from fribidi-unicode.h:

    BIDI_NUM_TYPES = 19 # Number of types defined in the bidi algorithm
    BIDI_MAX_EXPLICIT_LEVEL = 61 # maximum embedding level value assigned by explicit marks
    BIDI_MAX_RESOLVED_LEVELS = 63 # maximum *number* of different resolved embedding levels: 0-62

    # A few Unicode characters:

    # Bidirectional marks
    CHAR_LRM = 0x200E
    CHAR_RLM = 0x200F
    CHAR_LRE = 0x202A
    CHAR_RLE = 0x202B
    CHAR_PDF = 0x202C
    CHAR_LRO = 0x202D
    CHAR_RLO = 0x202E

    # Line and Paragraph Separators
    CHAR_LS = 0x2028
    CHAR_PS = 0x2029

    # Arabic Joining marks
    CHAR_ZWNJ = 0x200C
    CHAR_ZWJ = 0x200D

    # Hebrew and Arabic
    CHAR_HEBREW_ALEF = 0x05D0
    CHAR_ARABIC_ALEF = 0x0627
    CHAR_ARABIC_ZERO = 0x0660
    CHAR_PERSIAN_ZERO = 0x06F0

    # Misc
    CHAR_ZWNBSP = 0xFEFF

    # Char we place for a deleted slot, to delete later
    CHAR_FILL = CHAR_ZWNBSP

    # from fribidi-flags.h:

    # Define option flags that various functions use. Each mask has
    # only one bit set.

    FLAG_SHAPE_MIRRORING = 0x00000001
    FLAG_REORDER_NSM = 0x00000002

    FLAG_SHAPE_ARAB_PRES = 0x00000100
    FLAG_SHAPE_ARAB_LIGA = 0x00000200
    FLAG_SHAPE_ARAB_CONSOLE = 0x00000400

    FLAG_REMOVE_BIDI = 0x00010000
    FLAG_REMOVE_JOINING = 0x00020000
    FLAG_REMOVE_SPECIALS = 0x00040000

    # And their combinations.

    FLAGS_DEFAULT = FLAG_SHAPE_MIRRORING | FLAG_REORDER_NSM | FLAG_REMOVE_SPECIALS
    FLAGS_ARABIC = FLAG_SHAPE_ARAB_PRES | FLAG_SHAPE_ARAB_LIGA

    # from fribidi-bidi-types.h:

    # Define bit masks that bidi types are based on, each mask has
    # only one bit set.

    # RTL mask better be the least significant bit.
    MASK_RTL = 0x00000001 # Is right to left
    MASK_ARABIC = 0x00000002 # Is arabic

    # Each char can be only one of the three following.
    MASK_STRONG = 0x00000010 # Is strong
    MASK_WEAK = 0x00000020 # Is weak
    MASK_NEUTRAL = 0x00000040 # Is neutral
    MASK_SENTINEL = 0x00000080 # s sentinel
    # Sentinels are not valid chars, just identify the start/end of strings.

    # Each char can be only one of the five following.
    MASK_LETTER = 0x00000100 # Is letter: L, R, AL
    MASK_NUMBER = 0x00000200 # Is number: EN, AN
    MASK_NUMSEPTER = 0x00000400 # Is separator or terminator: ES, ET, CS
    MASK_SPACE = 0x00000800 # Is space: BN, BS, SS, WS
    MASK_EXPLICIT = 0x00001000 # Is explicit mark: LRE, RLE, LRO, RLO, PDF

    # Can be set only if FRIBIDI_MASK_SPACE is also set.
    MASK_SEPARATOR = 0x00002000 # Is text separator: BS, SS
    # Can be set only if FRIBIDI_MASK_EXPLICIT is also set.
    MASK_OVERRIDE = 0x00004000 # Is explicit override: LRO, RLO

    #  The following exist to make types pairwise different, some of them can
    # be removed but are here because of efficiency (make queries faster).

    MASK_ES = 0x00010000
    MASK_ET = 0x00020000
    MASK_CS = 0x00040000

    MASK_NSM = 0x00080000
    MASK_BN = 0x00100000

    MASK_BS = 0x00200000
    MASK_SS = 0x00400000
    MASK_WS = 0x00800000

    # We reserve a single bit for user's private use: we will never use it.
    MASK_PRIVATE = 0x01000000

    # Define values for FriBidiCharType

    # Strong types

    # Left-To-Right letter
    TYPE_LTR_VAL = MASK_STRONG | MASK_LETTER
    # Right-To-Left letter
    TYPE_RTL_VAL = MASK_STRONG | MASK_LETTER | MASK_RTL
    # Arabic Letter
    TYPE_AL_VAL = MASK_STRONG | MASK_LETTER | MASK_RTL | MASK_ARABIC
    # Left-to-Right Embedding
    TYPE_LRE_VAL = MASK_STRONG | MASK_EXPLICIT
    # Right-to-Left Embedding
    TYPE_RLE_VAL = MASK_STRONG | MASK_EXPLICIT | MASK_RTL
    # Left-to-Right Override
    TYPE_LRO_VAL = MASK_STRONG | MASK_EXPLICIT | MASK_OVERRIDE
    # Right-to-Left Override
    TYPE_RLO_VAL = MASK_STRONG | MASK_EXPLICIT | MASK_RTL | MASK_OVERRIDE

    # Weak types

    # Pop Directional Flag
    TYPE_PDF_VAL = MASK_WEAK | MASK_EXPLICIT
    # European Numeral
    TYPE_EN_VAL = MASK_WEAK | MASK_NUMBER
    # Arabic Numeral
    TYPE_AN_VAL = MASK_WEAK | MASK_NUMBER | MASK_ARABIC
    # European number Separator
    TYPE_ES_VAL = MASK_WEAK | MASK_NUMSEPTER | MASK_ES
    # European number Terminator
    TYPE_ET_VAL = MASK_WEAK | MASK_NUMSEPTER | MASK_ET
    # Common Separator
    TYPE_CS_VAL = MASK_WEAK | MASK_NUMSEPTER | MASK_CS
    # Non Spacing Mark
    TYPE_NSM_VAL = MASK_WEAK | MASK_NSM
    # Boundary Neutral
    TYPE_BN_VAL = MASK_WEAK | MASK_SPACE | MASK_BN

    # Neutral types

    # Block Separator
    TYPE_BS_VAL = MASK_NEUTRAL | MASK_SPACE | MASK_SEPARATOR | MASK_BS
    # Segment Separator
    TYPE_SS_VAL = MASK_NEUTRAL | MASK_SPACE | MASK_SEPARATOR | MASK_SS
    # WhiteSpace
    TYPE_WS_VAL = MASK_NEUTRAL | MASK_SPACE | MASK_WS
    # Other Neutral
    TYPE_ON_VAL = MASK_NEUTRAL

    # The following are used in specifying paragraph direction only.

    # Weak Left-To-Right
    TYPE_WLTR_VAL = MASK_WEAK
    # Weak Right-To-Left
    TYPE_WRTL_VAL = MASK_WEAK | MASK_RTL

    # start or end of text (run list) SENTINEL.  Only used internally
    TYPE_SENTINEL = MASK_SENTINEL

    # Private types for applications.  More private types can be obtained by
    # summing up from this one.
    TYPE_PRIVATE = MASK_PRIVATE

    # values for CharType:
    TYPE_LTR = TYPE_LTR_VAL
    TYPE_RTL = TYPE_RTL_VAL
    TYPE_AL = TYPE_AL_VAL
    TYPE_EN = TYPE_EN_VAL
    TYPE_AN = TYPE_AN_VAL
    TYPE_ES = TYPE_ES_VAL
    TYPE_ET = TYPE_ET_VAL
    TYPE_CS = TYPE_CS_VAL
    TYPE_NSM = TYPE_NSM_VAL
    TYPE_BN = TYPE_BN_VAL
    TYPE_BS = TYPE_BS_VAL
    TYPE_SS = TYPE_SS_VAL
    TYPE_WS = TYPE_WS_VAL
    TYPE_ON = TYPE_ON_VAL
    TYPE_LRE = TYPE_LRE_VAL
    TYPE_RLE = TYPE_RLE_VAL
    TYPE_LRO = TYPE_LRO_VAL
    TYPE_RLO = TYPE_RLO_VAL
    TYPE_PDF = TYPE_PDF_VAL

    # values for ParType:
    PAR_LTR = TYPE_LTR_VAL
    PAR_RTL = TYPE_RTL_VAL
    PAR_ON = TYPE_ON_VAL
    PAR_WLTR = TYPE_WLTR_VAL
    PAR_WRTL = TYPE_WRTL_VAL

    # Defining macros for needed queries, It is fully dependent on the
    # implementation of FriBidiCharType.

    def LEVEL_IS_RTL(lev) :
        "Is right-to-left level?"
        return \
            lev & 1 != 0
    #end LEVEL_IS_RTL

    def LEVEL_TO_DIR(lev) :
        "returns the bidi type corresponding to the direction of the level number."
        return \
            (FRIBIDI.TYPE_LTR, FRIBIDI.TYPE_RTL)[FRIBIDI.LEVEL_IS_RTL(lev)]
    #end LEVEL_TO_DIR

    def DIR_TO_LEVEL(dir) :
        "returns the minimum level of the direction, 0 for FRIBIDI_TYPE_LTR and" \
        "1 for FRIBIDI_TYPE_RTL and FRIBIDI_TYPE_AL."
        return \
            (0, 1)[FRIBIDI.IS_RTL(dir)]
    #end DIR_TO_LEVEL

    def IS_RTL(p) :
        "Is right to left: RTL, AL, RLE, RLO?"
        return \
            p & FRIBIDI.MASK_RTL != 0
    #end IS_RTL

    def IS_ARABIC(p) :
        "Is arabic: AL, AN?"
        return \
            p & FRIBIDI.MASK_ARABIC != 0
    #end IS_ARABIC

    def IS_STRONG(p) :
        "Is strong?"
        return \
            p & FRIBIDI.MASK_STRONG != 0
    #end IS_STRONG

    def IS_WEAK(p) :
        "Is weak?"
        return \
            p & FRIBIDI.MASK_WEAK != 0
    #end IS_WEAK

    def IS_NEUTRAL(p) :
        "Is neutral?"
        return \
            p & FRIBIDI.MASK_NEUTRAL != 0
    #end IS_NEUTRAL

    def IS_SENTINEL(p) :
        "Is sentinel?"
        return \
            p & FRIBIDI.MASK_SENTINEL != 0
    #end IS_SENTINEL

    def IS_LETTER(p) :
        "Is letter: L, R, AL?"
        return \
            p & FRIBIDI.MASK_LETTER != 0
    #end IS_LETTER

    def IS_NUMBER(p) :
        "Is number: EN, AN?"
        return \
            p & FRIBIDI.MASK_NUMBER != 0
    #end IS_NUMBER

    def IS_NUMBER_SEPARATOR_OR_TERMINATOR(p) :
        "Is number separator or terminator: ES, ET, CS?"
        return \
            p & FRIBIDI.MASK_NUMSEPTER != 0
    #end IS_NUMBER_SEPARATOR_OR_TERMINATOR

    def IS_SPACE(p) :
        "Is space: BN, BS, SS, WS?"
        return \
            p & FRIBIDI.MASK_SPACE != 0
    #end IS_SPACE

    def IS_EXPLICIT(p) :
        "Is explicit mark: LRE, RLE, LRO, RLO, PDF?"
        return \
            p & FRIBIDI.MASK_EXPLICIT != 0
    #end IS_EXPLICIT

    def IS_SEPARATOR(p) :
        "Is text separator: BS, SS?"
        return \
            p & FRIBIDI.MASK_SEPARATOR != 0
    #end IS_SEPARATOR

    def IS_OVERRIDE(p) :
        "Is explicit override: LRO, RLO?"
        return \
            p & FRIBIDI.MASK_OVERRIDE != 0
    #end IS_OVERRIDE

    # Some more:

    def IS_LTR_LETTER(p) :
        "Is left to right letter: LTR?"
        return \
            p & (FRIBIDI.MASK_LETTER | FRIBIDI.MASK_RTL) == FRIBIDI.MASK_LETTER
    #end IS_LTR_LETTER

    def IS_RTL_LETTER(p) :
        "Is right to left letter: RTL, AL?"
        return \
            p & (FRIBIDI.MASK_LETTER | FRIBIDI.MASK_RTL) == FRIBIDI.MASK_LETTER | FRIBIDI.MASK_RTL
    #end IS_RTL_LETTER

    def IS_ES_OR_CS(p) :
        "Is ES or CS: ES, CS?"
        return \
            p & (FRIBIDI.MASK_ES | FRIBIDI.MASK_CS) != 0
    #end IS_ES_OR_CS

    def IS_EXPLICIT_OR_BN(p) :
        "Is explicit or BN: LRE, RLE, LRO, RLO, PDF, BN?"
        return \
            p & (FRIBIDI.MASK_EXPLICIT | FRIBIDI.MASK_BN) != 0
    #end IS_EXPLICIT_OR_BN

    def IS_EXPLICIT_OR_BN_OR_NSM(p) :
        "Is explicit or BN or NSM: LRE, RLE, LRO, RLO, PDF, BN, NSM?"
        return \
            p & (FRIBIDI.MASK_EXPLICIT | FRIBIDI.MASK_BN | FRIBIDI.MASK_NSM) != 0
    #end IS_EXPLICIT_OR_BN_OR_NSM

    def IS_EXPLICIT_OR_BN_OR_WS(p) :
        "Is explicit or BN or WS: LRE, RLE, LRO, RLO, PDF, BN, WS?"
        return \
            p & (FRIBIDI.MASK_EXPLICIT | FRIBIDI.MASK_BN | FRIBIDI.MASK_WS) != 0
    #end IS_EXPLICIT_OR_BN_OR_WS

    def IS_EXPLICIT_OR_SEPARATOR_OR_BN_OR_WS(p) :
        "Is explicit or separator or BN or WS: LRE, RLE, LRO, RLO, PDF, BS, SS, BN, WS?"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_EXPLICIT
                    |
                        FRIBIDI.MASK_SEPARATOR
                    |
                        FRIBIDI.MASK_BN
                    |
                        FRIBIDI.MASK_WS
                    )
            !=
                0
            )
    #end IS_EXPLICIT_OR_SEPARATOR_OR_BN_OR_WS

    def IS_PRIVATE(p) :
        "Is private-use type for application?"
        return \
            p & FRIBIDI.MASK_PRIVATE != 0
    #end IS_PRIVATE

    # Define some conversions.

    def CHANGE_NUMBER_TO_RTL(p) :
        "changes numbers to RTL: EN,AN -> RTL."
        return \
            (p, FRIBIDI.TYPE_RTL)[FRIBIDI.IS_NUMBER(p)]
    #end CHANGE_NUMBER_TO_RTL

    def EXPLICIT_TO_OVERRIDE_DIR(p) :
        "overrides status of an explicit mark: LRO,LRE->LTR, RLO,RLE->RTL, otherwise->ON."
        return \
            (
                lambda : FRIBIDI.TYPE_ON,
                lambda : FRIBIDI.LEVEL_TO_DIR(FRIBIDI.DIR_TO_LEVEL(p)),
            )[FRIBIDI_IS_OVERRIDE(p)]()
    #end EXPLICIT_TO_OVERRIDE_DIR

    def WEAK_PARAGRAPH(p) :
        "weakens type for paragraph fallback purposes: LTR->WLTR, RTL->WRTL."
        return \
            FRIBIDI.PAR_WLTR | p & FRIBIDI.MASK_RTL
    #end WEAK_PARAGRAPH

    # more TBD

#end FRIBIDI

#+
# Routine arg/result types
#-

fribidi.fribidi_debug_status.restype = ct.c_int
fribidi.fribidi_debug_status.argtypes = ()
fribidi.fribidi_set_debug.restype = None
fribidi.fribidi_set_debug.argtypes = (ct.c_int,)
fribidi.fribidi_get_bidi_type.restype = FRIBIDI.CharType
fribidi.fribidi_get_bidi_type.argtypes = (FRIBIDI.Char,)
fribidi.fribidi_get_bidi_types.restype = None
fribidi.fribidi_get_bidi_types.argtypes = \
    (ct.POINTER(FRIBIDI.Char), FRIBIDI.StrIndex, ct.POINTER(FRIBIDI.CharType))
fribidi.fribidi_get_bidi_type_name.restype = ct.c_char_p
fribidi.fribidi_get_bidi_type_name.argtypes = (FRIBIDI.CharType,)

# more TBD

#+
# Higher-level stuff begins here
#-

def unicode_version() :
    "returns the supported Unicode version."
    return \
        ct.c_char_p.in_dll(fribidi, "fribidi_unicode_version").value.decode()
#end unicode_version

# from fribidi-bidi-types.h:

def get_bidi_type(ch) :
    "returns the bidi type of a character as defined in Table 3.7\n" \
    "Bidirectional Character Types of the Unicode Bidirectional Algorithm\n" \
    "available at\n"
    "http://www.unicode.org/reports/tr9/#Bidirectional_Character_Types, using\n" \
    "data provided in file UnicodeData.txt of the Unicode Character Database\n" \
    "available at http://www.unicode.org/Public/UNIDATA/UnicodeData.txt.\n" \
    "\n" \
    "There are a few macros defined in FRIBIDI for querying a bidi\n" \
    "type."
    return \
        fribidi.fribidi_get_bidi_type(ch)
#end get_bidi_type

def get_bidi_types(s) :
    "finds the bidi types of an string of characters. See\n" \
    "get_bidi_type() for more information about the bidi types returned\n" \
    "by this function."
    c_str = (FRIBIDI.Char * len(s))()
    for i in range(len(s)) :
        c_str[i] = ord(s[i])
    #end for
    c_result = (FRIBIDI.CharType * len(s))()
    fribidi.fribidi_get_bidi_types(c_str, len(s), c_result)
    return \
        tuple(c_result)
#end get_bidi_types

def get_bidi_type_name(t) :
    "returns the bidi type name of a character type.\n" \
    "\n" \
    "The type names are the same as ones defined in Table 3.7 Bidirectional\n" \
    "Character Types of the Unicode Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/#Bidirectional_Character_Types, with a\n" \
    "few modifications: L->LTR, R->RTL, B->BS, S->SS."
    return \
        fribidi.fribidi_get_bidi_type_name(t).decode()
#end get_bidi_type_name
