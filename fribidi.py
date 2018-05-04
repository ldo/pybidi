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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library, in a file named COPYING; if not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA
#-

import ctypes as ct

fribidi = ct.cdll.LoadLibrary("libfribidi.so.0")

def seq_to_ct(seq, ct_type, conv = None) :
    "extracts the elements of a Python sequence value into a ctypes array" \
    " of type ct_type, optionally applying the conv function to each value.\n" \
    "\n" \
    "Why doesn’t ctypes make this easy?"
    if conv == None :
        conv = lambda x : x
    #end if
    nr_elts = len(seq)
    result = (nr_elts * ct_type)()
    for i in range(nr_elts) :
        result[i] = conv(seq[i])
    #end for
    return \
        result
#end seq_to_ct

class FRIBIDI :
    "useful definitions adapted from fribidi/*.h. You will need to use the constants" \
    " and “macro” functions, but apart from that, see the more Pythonic wrappers" \
    " defined outside this class in preference to accessing low-level structures directly."

    # General ctypes gotcha: when passing addresses of ctypes-constructed objects
    # to routine calls, do not construct the objects directly in the call. Otherwise
    # the refcount goes to 0 before the routine is actually entered, and the object
    # can get prematurely disposed. Always store the object reference into a local
    # variable, and pass the value of the variable instead.

    # from fribidi-config.h:
    NAME = "GNU FriBidi"
    BUGREPORT = "http://bugs.freedesktop.org/enter_bug.cgi?product=fribidi"

    VERSION =  "0.19.7"
    MAJOR_VERSION = 0
    MINOR_VERSION = 19
    MICRO_VERSION = 7
    INTERFACE_VERSION = 3
    INTERFACE_VERSION_STRING = "3"

    Bool = ct.c_int # as per glib-2.0/glib/gtypes.h
    Level = ct.c_byte
    Char = ct.c_uint
    StrIndex = ct.c_int
    CharType = ct.c_uint
    ParType = ct.c_uint
    Flags = ct.c_uint
    JoiningType = ct.c_ubyte
    CharSet = ct.c_uint

    MAX_STRING_LENGTH = 0x7FFFFFFF

    # We do not support surrogates yet
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
    MASK_SENTINEL = 0x00000080 # Is sentinel
    # Sentinels are not valid chars, just identify the start/end of strings.

    # Each char can be only one of the five following.
    MASK_LETTER = 0x00000100 # Is letter: L, R, AL
    MASK_NUMBER = 0x00000200 # Is number: EN, AN
    MASK_NUMSEPTER = 0x00000400 # Is separator or terminator: ES, ET, CS
    MASK_SPACE = 0x00000800 # Is space: BN, BS, SS, WS
    MASK_EXPLICIT = 0x00001000 # Is explicit mark: LRE, RLE, LRO, RLO, PDF

    # Can be set only if MASK_SPACE is also set.
    MASK_SEPARATOR = 0x00002000 # Is text separator: BS, SS
    # Can be set only if MASK_EXPLICIT is also set.
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

    # start or end of text (run list) SENTINEL. Only used internally
    TYPE_SENTINEL = MASK_SENTINEL

    # Private types for applications. More private types can be obtained by
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
        "returns the minimum level of the direction, 0 for FRIBIDI.TYPE_LTR and" \
        "1 for FRIBIDI.TYPE_RTL and FRIBIDI.TYPE_AL."
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
            )[FRIBIDI.IS_OVERRIDE(p)]()
    #end EXPLICIT_TO_OVERRIDE_DIR

    def WEAK_PARAGRAPH(p) :
        "weakens type for paragraph fallback purposes: LTR->WLTR, RTL->WRTL."
        return \
            FRIBIDI.PAR_WLTR | p & FRIBIDI.MASK_RTL
    #end WEAK_PARAGRAPH

    # from fribidi-joining-types.h:

    # Define bit masks that joining types are based on, each mask has
    # only one bit set.

    MASK_JOINS_RIGHT = 0x01 #  May join to right
    MASK_JOINS_LEFT = 0x02 # May join to right
    MASK_ARAB_SHAPES = 0x04 # May Arabic shape
    MASK_TRANSPARENT = 0x08 # Is transparent
    MASK_IGNORED = 0x10 # Is ignored
    MASK_LIGATURED = 0x20 # Is ligatured

    # Define values for JoiningType

    # nUn-joining
    JOINING_TYPE_U_VAL = 0
    # Right-joining
    JOINING_TYPE_R_VAL = MASK_JOINS_RIGHT | MASK_ARAB_SHAPES
    # Dual-joining
    JOINING_TYPE_D_VAL = MASK_JOINS_RIGHT | MASK_JOINS_LEFT | MASK_ARAB_SHAPES
    # join-Causing
    JOINING_TYPE_C_VAL = MASK_JOINS_RIGHT | MASK_JOINS_LEFT
    # Left-joining
    JOINING_TYPE_L_VAL = MASK_JOINS_LEFT | MASK_ARAB_SHAPES
    # Transparent
    JOINING_TYPE_T_VAL = MASK_TRANSPARENT | MASK_ARAB_SHAPES
    # iGnored
    JOINING_TYPE_G_VAL = MASK_IGNORED

    JOINING_TYPE_U = JOINING_TYPE_U_VAL
    JOINING_TYPE_R = JOINING_TYPE_R_VAL
    JOINING_TYPE_D = JOINING_TYPE_D_VAL
    JOINING_TYPE_C = JOINING_TYPE_C_VAL
    JOINING_TYPE_T = JOINING_TYPE_T_VAL
    JOINING_TYPE_L = JOINING_TYPE_L_VAL
    JOINING_TYPE_G = JOINING_TYPE_G_VAL

    # ArabicProp is essentially the same type as JoiningType, but
    # not limited to the few values returned by fribidi_get_joining_type.
    ArabicProp = JoiningType

    # The equivalent of JoiningType values for ArabicProp

    # Primary Arabic Joining Classes (Table 8-2)

    def IS_JOINING_TYPE_U(p) :
        "nUn-joining"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_TRANSPARENT
                    |
                        FRIBIDI.MASK_IGNORED
                    |
                        FRIBIDI.MASK_JOINS_RIGHT
                    |
                        FRIBIDI.MASK_JOINS_LEFT
                    )
            ==
                0
            )
    #end IS_JOINING_TYPE_U

    def IS_JOINING_TYPE_R(p) :
        "Right-joining"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_TRANSPARENT
                    |
                        FRIBIDI.MASK_IGNORED
                    |
                        FRIBIDI.MASK_JOINS_RIGHT
                    |
                        FRIBIDI.MASK_JOINS_LEFT
                    )
            ==
                FRIBIDI.MASK_JOINS_RIGHT
            )
    #end IS_JOINING_TYPE_R

    def IS_JOINING_TYPE_D(p) :
        "Dual-joining"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_TRANSPARENT
                    |
                        FRIBIDI.MASK_IGNORED
                    |
                        FRIBIDI.MASK_JOINS_RIGHT
                    |
                        FRIBIDI.MASK_JOINS_LEFT
                    |
                        FRIBIDI.MASK_ARAB_SHAPES
                    )
            ==
                FRIBIDI.MASK_JOINS_RIGHT | FRIBIDI.MASK_JOINS_LEFT | FRIBIDI.MASK_ARAB_SHAPES
            )
    #end IS_JOINING_TYPE_D

    def IS_JOINING_TYPE_C(p) :
        "join-Causing"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_TRANSPARENT
                    |
                        FRIBIDI.MASK_IGNORED
                    |
                        FRIBIDI.MASK_JOINS_RIGHT
                    |
                        FRIBIDI.MASK_JOINS_LEFT
                    |
                        FRIBIDI.MASK_ARAB_SHAPES
                    )
            ==
                FRIBIDI.MASK_JOINS_RIGHT | FRIBIDI.MASK_JOINS_LEFT
            )
    #end IS_JOINING_TYPE_C

    def IS_JOINING_TYPE_L(p) :
        "Left-joining"
        return \
            (
                    p
                &
                    (
                        FRIBIDI.MASK_TRANSPARENT
                    |
                        FRIBIDI.MASK_IGNORED
                    |
                        FRIBIDI.MASK_JOINS_RIGHT
                    |
                        FRIBIDI.MASK_JOINS_LEFT
                    )
            ==
                FRIBIDI.MASK_JOINS_LEFT
            )
    #end IS_JOINING_TYPE_L

    def IS_JOINING_TYPE_T(p) :
        "Transparent"
        return \
            (
                p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED)
            ==
                FRIBIDI.MASK_TRANSPARENT
            )
    #end IS_JOINING_TYPE_T

    def IS_JOINING_TYPE_G(p) :
        "iGnored"
        return \
            (
                p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED)
            ==
                FRIBIDI.MASK_IGNORED
            )
    #end IS_JOINING_TYPE_G

    # and for Derived Arabic Joining Classes (Table 8-3)

    def IS_JOINING_TYPE_RC(p) :
        "Right join-Causing"
        return \
            (
                p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED | FRIBIDI.MASK_JOINS_RIGHT)
            ==
                FRIBIDI.MASK_JOINS_RIGHT
            )
    #end IS_JOINING_TYPE_RC

    def IS_JOINING_TYPE_LC(p) :
        "Left join-Causing"
        return \
            (
                p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED | FRIBIDI.MASK_JOINS_LEFT)
            ==
                FRIBIDI.MASK_JOINS_LEFT
            )
    #end IS_JOINING_TYPE_LC

    # Defining macros for needed queries, It is fully dependent on the
    # implementation of FriBidiJoiningType.

    def JOINS_RIGHT(p) :
        "Joins to right: R, D, C?"
        return \
            p & FRIBIDI.MASK_JOINS_RIGHT != 0
    #end JOINS_RIGHT

    def JOINS_LEFT(p) :
        "Joins to left: L, D, C?"
        return \
            p & FRIBIDI.MASK_JOINS_LEFT != 0
    #end JOINS_LEFT

    def ARAB_SHAPES(p) :
        "May shape: R, D, L, T?"
        return \
            p & FRIBIDI.MASK_ARAB_SHAPES != 0
    #end ARAB_SHAPES

    def IS_JOIN_SKIPPED(p) :
        "Is skipped in joining: T, G?"
        return \
            p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED) != 0
    #end IS_JOIN_SKIPPED

    def IS_JOIN_BASE_SHAPES(p) :
        "Is base that will be shaped: R, D, L?"
        return \
            (
                p & (FRIBIDI.MASK_TRANSPARENT | FRIBIDI.MASK_IGNORED | FRIBIDI.MASK_ARAB_SHAPES)
            ==
                FRIBIDI.MASK_ARAB_SHAPES
            )
    #end IS_JOIN_BASE_SHAPES

    def JOINS_PRECEDING_MASK(level) :
        return \
            (FRIBIDI.MASK_JOINS_LEFT, FRIBIDI.MASK_JOINS_RIGHT)[FRIBIDI.LEVEL_IS_RTL(level)]
    #end JOINS_PRECEDING_MASK

    def JOINS_FOLLOWING_MASK(level) :
        return \
            (FRIBIDI.MASK_JOINS_RIGHT, FRIBIDI.MASK_JOINS_LEFT)[FRIBIDI.LEVEL_IS_RTL(level)]
    #end JOINS_FOLLOWING_MASK

    def JOIN_SHAPE(p) :
        return \
            p & (FRIBIDI.MASK_JOINS_RIGHT | FRIBIDI.MASK_JOINS_LEFT) != 0
    #end JOIN_SHAPE

    # from fribidi-char-sets.h:

    CHAR_SET_NOT_FOUND = 0
    CHAR_SET_UTF8 = 1
    CHAR_SET_CAP_RTL = 2
    CHAR_SET_ISO8859_6 = 3 # ISO8859-6 (Arabic)
    CHAR_SET_ISO8859_8 = 4 # ISO8859-8 (Hebrew)
    CHAR_SET_CP1255 = 5 # CP1255 (MS Hebrew/Yiddish)
    CHAR_SET_CP1256 = 6 # CP1256 (MS Arabic)
    CHAR_SETS_NUM_PLUS_ONE = 7
    CHAR_SETS_NUM = CHAR_SETS_NUM_PLUS_ONE - 1

#end FRIBIDI
FB = FRIBIDI # if you prefer

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
fribidi.fribidi_get_par_direction.restype = FRIBIDI.ParType
fribidi.fribidi_get_par_direction.argtypes = (ct.POINTER(FRIBIDI.CharType), FRIBIDI.StrIndex)
fribidi.fribidi_get_par_embedding_levels.restype = FRIBIDI.Level
fribidi.fribidi_get_par_embedding_levels.argtypes = \
    (
        ct.POINTER(FRIBIDI.CharType), # input list of bidi types as returned by fribidi_get_bidi_types()
        FRIBIDI.StrIndex, # input string length of the paragraph
        ct.POINTER(FRIBIDI.ParType), # requested and resolved paragraph base direction
        ct.POINTER(FRIBIDI.Level), # output list of embedding levels
    )
fribidi.fribidi_reorder_line.restype = FRIBIDI.Level
fribidi.fribidi_reorder_line.argtypes = \
    (
        FRIBIDI.Flags, # reorder flags
        ct.POINTER(FRIBIDI.CharType), # input list of bidi types as returned by fribidi_get_bidi_types()
        FRIBIDI.StrIndex, # input length of the paragraph
        FRIBIDI.StrIndex, # input offset of the beginning of the line in the paragraph
        FRIBIDI.ParType, # resolved paragraph base direction
        ct.POINTER(FRIBIDI.Level),
          # input list of embedding levels, as returned by fribidi_get_par_embedding_levels
        ct.POINTER(FRIBIDI.Char), # paragraph visual string to reorder
        ct.POINTER(FRIBIDI.StrIndex),
          # a map of string indices which is reordered to reflect where each glyph ends up.
    )
fribidi.fribidi_get_joining_type.restype = FRIBIDI.JoiningType
fribidi.fribidi_get_joining_type.argtypes = (FRIBIDI.Char,)
fribidi.fribidi_get_joining_types.restype = None
fribidi.fribidi_get_joining_types.argtypes = \
    (ct.POINTER(FRIBIDI.Char), FRIBIDI.StrIndex, ct.POINTER(FRIBIDI.JoiningType))
fribidi.fribidi_get_joining_type_name.restype = ct.c_char_p
fribidi.fribidi_get_joining_type_name.argtypes = (FRIBIDI.JoiningType,)
fribidi.fribidi_join_arabic.restype = None
fribidi.fribidi_join_arabic.argtypes = \
    (
        ct.POINTER(FRIBIDI.CharType),
          # input list of bidi types as returned by fribidi_get_bidi_types()
        FRIBIDI.StrIndex, # input string length
        ct.POINTER(FRIBIDI.Level),
          # input list of embedding levels, as returned by fribidi_get_par_embedding_levels()
        ct.POINTER(FRIBIDI.ArabicProp),
          # Arabic properties to analyze, initialized by joining types, as
          # returned by fribidi_get_joining_types()
    )
fribidi.fribidi_get_mirror_char.restype = FRIBIDI.Bool
fribidi.fribidi_get_mirror_char.argtypes = (FRIBIDI.Char, ct.POINTER(FRIBIDI.Char))
fribidi.fribidi_shape_mirroring.restype = None
fribidi.fribidi_shape_mirroring.argtypes = \
    (ct.POINTER(FRIBIDI.Level), FRIBIDI.StrIndex, ct.POINTER(FRIBIDI.Char))
fribidi.fribidi_shape_arabic.restype = None
fribidi.fribidi_shape_arabic.argtypes = \
    (
        FRIBIDI.Flags, # shaping flags
        ct.POINTER(FRIBIDI.Level), # embedding_levels
        FRIBIDI.StrIndex, # input string length
        ct.POINTER(FRIBIDI.ArabicProp),
          # input/output Arabic properties as computed by fribidi_join_arabic
        ct.POINTER(FRIBIDI.Char), # string to shape
    )
fribidi.fribidi_shape.restype = None
fribidi.fribidi_shape.argtypes = \
    (
        FRIBIDI.Flags, # shaping flags
        ct.POINTER(FRIBIDI.Level), # embedding_levels
        FRIBIDI.StrIndex, # input string length
        ct.POINTER(FRIBIDI.ArabicProp),
          # input/output Arabic properties as computed by fribidi_join_arabic
        ct.POINTER(FRIBIDI.Char), # string to shape
    )
fribidi.fribidi_charset_to_unicode.restype = FRIBIDI.StrIndex
fribidi.fribidi_charset_to_unicode.argtypes = \
    (FRIBIDI.CharSet, ct.c_char_p, FRIBIDI.StrIndex, ct.POINTER(FRIBIDI.Char))
fribidi.fribidi_unicode_to_charset.restype = FRIBIDI.StrIndex
fribidi.fribidi_unicode_to_charset.argtypes = \
    (FRIBIDI.CharSet, ct.POINTER(FRIBIDI.Char), FRIBIDI.StrIndex, ct.c_char_p)
fribidi.fribidi_parse_charset.restype = FRIBIDI.CharSet
fribidi.fribidi_parse_charset.argtypes = (ct.c_char_p,)
fribidi.fribidi_char_set_name.restype = ct.c_char_p
fribidi.fribidi_char_set_name.argtypes = (FRIBIDI.CharSet,)
fribidi.fribidi_char_set_title.restype = ct.c_char_p
fribidi.fribidi_char_set_title.argtypes = (FRIBIDI.CharSet,)
fribidi.fribidi_char_set_desc.restype = ct.c_char_p
fribidi.fribidi_char_set_desc.argtypes = (FRIBIDI.CharSet,)
fribidi.fribidi_remove_bidi_marks.restype = FRIBIDI.StrIndex
fribidi.fribidi_remove_bidi_marks.argtyes = \
    (
        ct.POINTER(FRIBIDI.Char), # input string to clean
        FRIBIDI.StrIndex, # input string length
        ct.POINTER(FRIBIDI.StrIndex),
          # positions_to_this -- list mapping positions to the order used in str
        ct.POINTER(FRIBIDI.StrIndex),
          # position_from_this_list -- list mapping positions from the order used in str
        ct.POINTER(FRIBIDI.Level),
          # embedding_levels -- list of embedding levels
    )
fribidi.fribidi_log2vis.restype = FRIBIDI.Level
fribidi.fribidi_log2vis.argtypes = \
    (
        ct.POINTER(FRIBIDI.Char), # input logical string
        FRIBIDI.StrIndex, # input string length
        ct.POINTER(FRIBIDI.ParType), # requested and resolved paragraph base direction
        ct.POINTER(FRIBIDI.Char), # output visual string
        ct.POINTER(FRIBIDI.StrIndex), # output mapping from logical to visual string positions
        ct.POINTER(FRIBIDI.StrIndex),
          # output mapping from visual string back to the logical string positions
        ct.POINTER(FRIBIDI.StrIndex), # output list of embedding levels
    )

#+
# Higher-level stuff begins here
#-

def str_to_chars(s) :
    "returns the characters of the Python string s as a ctypes array of FRIBIDI.Char."
    return \
        seq_to_ct(s, FRIBIDI.Char, ord)
#end str_to_chars

def chars_to_str(s) :
    "returns the characters of a ctypes array of FRIBIDI.Char as a Python string."
    return \
        "".join(chr(c) for c in s)
#end chars_to_str

# from fribidi-unicode.h:

def get_unicode_version() :
    "returns the supported Unicode version."
    return \
        ct.c_char_p.in_dll(fribidi, "fribidi_unicode_version").value.decode()
#end get_unicode_version

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
    c_str = str_to_chars(s)
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

# from fribidi-bidi.h:

def get_par_direction(bidi_types) :
    "finds the base direction of a single paragraph,\n" \
    "as defined by rule P2 of the Unicode Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/#P2.\n" \
    "\n" \
    "You typically do not need this function as\n" \
    "get_par_embedding_levels() knows how to compute base direction\n" \
    "itself, but you may need this to implement a more sophisticated paragraph\n" \
    "direction handling. Note that you can pass more than a paragraph to this\n" \
    "function and the direction of the first non-neutral paragraph is returned,\n" \
    "which is a very good heuristic to set direction of the neutral paragraphs\n" \
    "at the beginning of text. For other neutral paragraphs, you better use the\n" \
    "direction of the previous paragraph.\n" \
    "\n" \
    "Returns: Base paragraph direction. No weak paragraph direction is returned,\n" \
    "only LTR, RTL, or ON."
    c_bidi_types = seq_to_ct(bidi_types, FRIBIDI.CharType)
    return \
        fribidi.fribidi_get_par_direction(c_bidi_types, len(bidi_types))
#end get_par_direction

def get_par_embedding_levels(bidi_types, pbase_dir) :
    "finds the bidi embedding levels of a single paragraph,\n" \
    "as defined by the Unicode Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/. This function implements rules P2 to\n" \
    "I1 inclusive, and parts 1 to 3 of L1, except for rule X9 which is\n" \
    "implemented in fribidi_remove_bidi_marks(). Part 4 of L1 is implemented\n" \
    "in fribidi_reorder_line().\n" \
    "\n" \
    "There are a few macros defined in FRIBIDI to work with this\n" \
    "embedding levels.\n" \
    "\n" \
    "Returns a 3-tuple: (Maximum level found plus one, resolved paragraph base direction," \
    " tuple of embedding levels)."
    nr_bidi_types = len(bidi_types)
    c_bidi_types = seq_to_ct(bidi_types, FRIBIDI.CharType)
    c_pbase_dir = FRIBIDI.ParType(pbase_dir)
    c_levels = (FRIBIDI.Level * nr_bidi_types)()
    max_level = fribidi.fribidi_get_par_embedding_levels(c_bidi_types, nr_bidi_types, ct.byref(c_pbase_dir), c_levels)
    if max_level == 0 :
        raise RuntimeError("fribidi_get_par_embedding_levels returned 0")
          # out of memory?
    #end if
    return \
        max_level, c_pbase_dir.value, tuple(c_levels)
#end get_par_embedding_levels

class Reordering :
    "convenience class for reordering elements of a sequence."

    __slots__ = \
        (
            "indexes", # tuple of remapped position indices
        )

    def __init__(self, length, elt_func) :
        "length is the number of elements, and elt_func is a function which," \
        " given each input index, returns the appropriate remapped index."
        self.indexes = tuple(elt_func(i) for i in range(length))
        assert set(self.indexes) == set(range(length)), "reordering omits or duplicates items"
          # each possible index must occur once and only once
    #end __init__

    @classmethod
    def identity(celf, length) :
        "constructs an identity Reordering of the specified length." \
        " Instead of an integer, you can pass a sequence (bytes," \
        " list, str, tuple), and the length will be taken from" \
        " that of the sequence."
        if isinstance(length, (bytes, list, str, tuple)) :
            length = len(length)
        #end if
        return \
            celf(length = length, elt_func = lambda i : i)
    #end identity

    @classmethod
    def from_seq(celf, seq) :
        "constructs a Reordering taking its indexes from the specified sequence." \
        " Besides a list or tuple, you can also pass a ctypes array, for example" \
        " of FRIBIDI.StrIndex elements."
        return \
            celf(length = len(seq), elt_func = lambda i : seq[i])
    #end from_seq

    def inverse(self) :
        "returns the inverse of this Reordering."
        indexes = [None] * len(self)
        for i in range(len(self)) :
            indexes[self.indexes[i]] = i
        #end for
        return \
            type(self).from_seq(indexes)
    #end inverse

    def __len__(self) :
        return \
            len(self.indexes)
    #end __len__

    def apply(self, seq, offset = 0) :
        "applies the Reordering to the specified sequence (bytes, list, string, tuple)" \
        " beginning at the specified offset. Any part of the sequence prior to the offset," \
        " and beyond the length of the Reordering, remains unchanged."
        if not isinstance(seq, (bytes, list, str, tuple)) :
            raise TypeError("invalid type “{}” for applying Reordering".format(type(seq).__name__))
        #end if
        if len(seq) < offset + len(self) :
            raise IndexError \
              (
                "sequence length {} is too short for my Reordering length {} at offset {}"
                .format(len(seq), len(self), offset)
              )
        #end if
        if isinstance(seq, str) :
            seq = tuple(ord(c) for c in seq)
            result_convert = lambda l : "".join(chr(c) for c in l)
              # convert back to string
        else :
            result_convert = None
        #end if
        prefix = seq[:offset]
        postfix = seq[offset + len(self):]
        seq = seq[offset : offset + len(self)]
        result = type(seq)(seq[self.indexes[i]] for i in range(len(self)))
        result = prefix + result + postfix
        if result_convert != None :
            result = result_convert(result)
        #end if
        return \
            result
    #end apply

    def to_ct(self) :
        "returns the indexes as a ctypes array of FRIBIDI.StrIndex values."
        return \
            seq_to_ct(self.indexes, FRIBIDI.StrIndex)
    #end to_ct

    def __repr__(self) :
        return \
            "<{}>{}".format(type(self).__name__, repr(self.indexes))
    #end __repr__

#end Reordering

def reorder_line(flags, bidi_types, line_offset, base_dir, embedding_levels, logical_str, map = None) :
    "reorders the characters in a line of text from logical to\n" \
    "final visual order. This function implements part 4 of rule L1, and rules\n" \
    "L2 and L3 of the Unicode Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/#Reordering_Resolved_Levels.\n" \
    "It will also compute a new Reordering if one was passed.\n" \
    "\n" \
    "You should provide the resolved paragraph direction and embedding levels as\n" \
    "set by get_par_embedding_levels(). Also note that the embedding\n" \
    "levels may change a bit. To be exact, the embedding level of any sequence\n" \
    "of white space at the end of line is reset to the paragraph embedding level\n" \
    "(That is part 4 of rule L1).\n" \
    "\n" \
    "Note that the bidi types and embedding levels are not reordered. You can\n" \
    "reorder these (or any other) arrays using the map later. The user is\n" \
    "responsible to initialize map to something sensible, like an identity\n" \
    "Reordering, or pass None if no map is needed.\n" \
    "\n" \
    "There is an optional part to this function, which is whether non-spacing\n" \
    "marks for right-to-left parts of the text should be reordered to come after\n" \
    "their base characters in the visual string or not. Most rendering engines\n" \
    "expect this behaviour, but console-based systems for example do not like it.\n" \
    "This is controlled by the FRIBIDI.FLAG_REORDER_NSM flag. The flag is on\n" \
    "in FRIBIDI.FLAGS_DEFAULT.\n" \
    "\n" \
    "Returns a 2- or 3-tuple: (Maximum level found in this line plus one, reordered string, reordered map if specified)"
    para_len = len(bidi_types)
    for seq, seq_name in \
        (
            (embedding_levels, "embedding_levels"),
            (logical_str, "logical_str"),
        ) \
    :
        assert para_len == len(seq), "lengths of bidi_types and {} disagree".format(seq_name)
    #end for
    c_bidi_types = seq_to_ct(bidi_types, FRIBIDI.CharType)
    c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.Level)
    c_str = str_to_chars(logical_str)
    if map != None :
        if isinstance(map, Reordering) :
            assert len(map) == para_len, "length of Reordering map disagrees with para len"
            c_map = map.to_ct()
        elif isinstance(map, (list, tuple)) :
            assert len(map) == para_len, "length of map list/tuple disagrees with para len"
            c_map = seq_to_ct(map, FRIBIDI.StrIndex)
        else :
            raise TypeError("invalid type for map")
        #end if
    else :
        c_map = None
    #end if
    max_level = fribidi.fribidi_reorder_line \
      (flags, c_bidi_types, para_len, line_offset, base_dir, c_embedding_levels, c_str, c_map)
    if max_level == 0 :
        raise RuntimeError("fribidi_reorder_line returned 0")
          # out of memory?
    #end if
    result = (max_level, chars_to_str(c_str))
    if map != None :
        if isinstance(map, Reordering) :
            map = Reordering.from_seq(c_map)
        else :
            map = type(map)(c_map)
        result += (map,)
    #end if
    return \
         result
#end reorder_line

# from fribidi-joining-types.h:

def get_joining_type(ch) :
    "returns the joining type of a character as defined in Table\n" \
    "8-2 Primary Arabic Joining Classes of the Unicode standard available at\n" \
    "http://www.unicode.org/versions/Unicode4.0.0/ch08.pdf#G7462, using data\n" \
    "provided in file ArabicShaping.txt and UnicodeData.txt of the Unicode\n" \
    "Character Database available at\n" \
    "http://www.unicode.org/Public/UNIDATA/ArabicShaping.txt and\n" \
    "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt.\n" \
    "\n" \
    "There are a few macros defined in FRIBIDI for querying a\n" \
    "joining type."
    return \
        fribidi.fribidi_get_joining_type(ch)
#end get_joining_type

def get_joining_types(s) :
    "finds the joining types of an string of characters. See\n" \
    "fribidi_get_joining_type for more information about the joining types\n" \
    "returned by this function."
    c_str = str_to_chars(s)
    jtypes = (len(s) * FRIBIDI.JoiningType)()
    fribidi.fribidi_get_joining_types(c_str, len(s), jtypes)
    return \
        tuple(jtypes)
#end get_joining_types

def get_joining_type_name(j) :
    "returns the joining type name of a joining type. The type names are\n" \
    "the same as ones defined in Table 8-2  Primary Arabic\n" \
    "Joining Classes of the Unicode standard available at\n" \
    "http://www.unicode.org/versions/Unicode4.0.0/ch08.pdf#G7462."
    return \
        fribidi.fribidi_get_joining_type_name(j).decode()
#end get_joining_type_name

# from fribidi-joining.h:

def join_arabic(bidi_types, embedding_levels, ar_props) :
    "does the Arabic joining algorithm. Means, given Arabic\n" \
    "joining types of the characters in ar_props, this\n" \
    "function returns a modified copy of these properties to grasp\n" \
    "the effect of neighbouring characters. You probably need this\n" \
    "information later to do Arabic shaping.\n" \
    "\n" \
    "This function implements rules R1 to R7 inclusive (all rules) of the Arabic\n" \
    "Cursive Joining algorithm of the Unicode standard as available at\n" \
    "http://www.unicode.org/versions/Unicode4.0.0/ch08.pdf#G7462. It also\n" \
    "interacts correctly with the bidirection algorithm as defined in Section\n" \
    "3.5 Shaping of the Unicode Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/#Shaping.\n" \
    "\n" \
    "There are a few macros defined in FRIBIDI for querying the\n" \
    "Arabic properties computed by this function."
    str_len = len(bidi_types)
    assert str_len == len(embedding_levels) and str_len == len(ar_props), \
        "inconsistent lengths of input sequences"
    c_bidi_types = seq_to_ct(bidi_types, FRIBIDI.CharType)
    c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.Level)
    c_ar_props = seq_to_ct(ar_props, FRIBIDI.ArabicProp)
    fribidi.fribidi_join_arabic(c_bidi_types, str_len, c_embedding_levels, c_ar_props)
    return \
        type(ar_props)(c_ar_props)
#end join_arabic

# from fribidi-mirroring.h:

def get_mirror_char(ch) :
    "finds the mirrored equivalent of a character as defined in\n" \
    "the file BidiMirroring.txt of the Unicode Character Database available at\n" \
    "http://www.unicode.org/Public/UNIDATA/BidiMirroring.txt.\n" \
    "\n" \
    "If the input character is a declared as a mirroring character in the\n" \
    "Unicode standard and has a mirrored equivalent. The matching mirrored\n" \
    "character is put in the output, otherwise the input character itself is\n" \
    "put.\n" \
    "\n" \
    "Returns: the mirrored character if it exists, else None."
    mirrored_ch = FRIBIDI.Char()
    if fribidi.fribidi_get_mirror_char(ch, ct.byref(mirrored_ch)) :
        result = mirrored_ch.value
    else :
        result = None
    #end if
    return \
        result
#end get_mirror_char

def shape_mirroring(embedding_levels, string) :
    "replaces mirroring characters on right-to-left embeddings in\n" \
    "string with their mirrored equivalent as returned by\n" \
    "get_mirror_char().\n" \
    "\n" \
    "This function implements rule L4 of the Unicode Bidirectional Algorithm\n" \
    "available at http://www.unicode.org/reports/tr9/#L4."
    c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.Level)
    c_str = str_to_chars(string)
    fribidi.fribidi_shape_mirroring(c_embedding_levels, len(string), c_str)
    return \
        chars_to_str(c_str)
#end shape_mirroring

# from fribidi-arabic.h:

def shape_arabic(flags, embedding_levels, ar_props, string) :
    "does Arabic shaping\n" \
    "\n" \
    "The actual shaping that is done depends on the flags set. Only flags\n" \
    "starting with FRIBIDI.FLAG_SHAPE_ARAB_ affect this function.\n" \
    "Currently these are:\n" \
    "\n" \
    "* FRIBIDI.FLAG_SHAPE_MIRRORING: Do mirroring.\n" \
    "* FRIBIDI.FLAG_SHAPE_ARAB_PRES: Shape Arabic characters to their\n" \
    "               presentation form glyphs.\n" \
    "* FRIBIDI.FLAG_SHAPE_ARAB_LIGA: Form mandatory Arabic ligatures.\n" \
    "* FRIBIDI.FLAG_SHAPE_ARAB_CONSOLE: Perform additional Arabic shaping\n" \
    "                  suitable for text rendered on\n" \
    "                  grid terminals with no mark\n" \
    "                  rendering capabilities.\n" \
    "\n" \
    "Of the above, FRIBIDI.FLAG_SHAPE_ARAB_CONSOLE is only used in special\n" \
    "cases, but the rest are recommended in any enviroment that doesn't have\n" \
    "other means for doing Arabic shaping.The set of extra flags that enable\n" \
    "this level of Arabic support has a shortcut named FRIBIDI.FLAGS_ARABIC."
    c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.Level)
    c_ar_props = seq_to_ct(ar_props, FRIBIDI.ArabicProp)
    c_str = str_to_chars(string)
    fribidi.fribidi_shape_arabic(flags, c_embedding_levels, len(string), c_ar_props, c_str)
    return \
        type(ar_props)(c_ar_props), chars_to_str(c_str)
#end shape_arabic

# from fribidi-shape.h:

def shape(flags, embedding_levels, ar_props, string) :
    "does all shaping work that depends on the resolved embedding\n" \
    "levels of the characters. Currently it does mirroring and Arabic shaping,\n" \
    "but the list may grow in the future. This function is a wrapper around\n" \
    "fribidi_shape_mirroring and fribidi_shape_arabic.\n" \
    "\n" \
    "The flags parameter specifies which shapings are applied. The only flags\n" \
    "affecting the functionality of this function are those beginning with\n" \
    "FRIBIDI.FLAG_SHAPE_. Of these, only FRIBIDI.FLAG_SHAPE_MIRRORING is on\n" \
    "in FRIBIDI.FLAGS_DEFAULT. For details of the Arabic-specific flags see\n" \
    "fribidi_shape_arabic. If ar_props is NULL, no Arabic shaping is performed.\n" \
    "\n" \
    "Feel free to do your own shaping before or after calling this function,\n" \
    "but you should take care of embedding levels yourself then."
    c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.Level)
    c_ar_props = seq_to_ct(ar_props, FRIBIDI.ArabicProp)
    c_str = str_to_chars(string)
    fribidi.fribidi_shape(flags, c_embedding_levels, len(string), c_ar_props, c_str)
    return \
        type(ar_props)(c_ar_props), chars_to_str(c_str)
#end shape

# from fribidi-char-sets.h:
# Seems the conversion functions cannot tell me how many characters or bytes they
# produce, I simply have to provide a large buffer and hope for the best!
length_factor = 5 # hope this is enough...

def charset_to_unicode(char_set, s_bytes) :
    "converts a bytes value from a character set, to a Unicode string."
    us = (len(s_bytes) * length_factor * FRIBIDI.Char)()
    strlen = fribidi.fribidi_charset_to_unicode(char_set, s_bytes, len(s_bytes), us)
    if strlen > len(us) :
        raise IndexError("PANIC! buffer overflow!") # if I haven’t crashed first...
    #end if
    return \
        chars_to_str(us[:strlen])
#end charset_to_unicode

def unicode_to_charset(char_set, us) :
    "converts a Unicode string to a bytes value in another character set."
    c_us = str_to_chars(us)
    s_bytes = (len(us) * length_factor * ct.c_char)()
    s_len = fribidi.fribidi_unicode_to_charset(char_set, c_us, len(us), s_bytes)
    if s_len >= len(s_bytes) :
        raise IndexError("PANIC! buffer overflow!") # if I haven’t crashed first...
    #end if
    return \
        s_bytes.value # automatically stops at NUL
#end unicode_to_charset

def parse_charset(s) :
    "parses character set name\n" \
    "\n" \
    "Returns: The character set named s, or FRIBIDI.CHAR_SET_NOT_FOUND if the\n" \
    "character set is not available."
    c_s = s.encode()
    return \
        fribidi.fribidi_parse_charset(c_s)
#end parse_charset

def char_set_name(char_set) :
    return \
        fribidi.fribidi_char_set_name(char_set).decode()
#end char_set_name

def char_set_title(char_set) :
    return \
        fribidi.fribidi_char_set_title(char_set).decode()
#end char_set_title

def char_set_desc(char_set) :
    desc = fribidi.fribidi_char_set_desc(char_set)
    if desc != None :
        desc = desc.decode()
    #end if
    return \
        desc
#end char_set_desc

# selected decls from fribidi-deprecated.h:
# (No other way to get these particular items of functionality that I can see)

def remove_bidi_marks(string, positions_to_this = None, position_from_this_list = None, embedding_levels = None) :
    "removes the bidi and boundary-neutral marks out of a string\n" \
    "and the accompanying lists. It implements rule X9 of the Unicode\n" \
    "Bidirectional Algorithm available at\n" \
    "http://www.unicode.org/reports/tr9/#X9, with the exception that it removes\n" \
    "U+200E LEFT-TO-RIGHT MARK and U+200F RIGHT-TO-LEFT MARK too.\n" \
    "\n" \
    "If any of the input lists are None, the list is skipped. If str is the\n" \
    "visual string, then positions_to_this is positions_L_to_V and\n" \
    "position_from_this_list is positions_V_to_L; if str is the logical\n" \
    "string, the other way. Moreover, the position maps should be filled with\n" \
    "valid entries.\n" \
    "\n" \
    "A position map pointing to a removed character is filled with \-1. By the\n" \
    "way, you should not use embedding_levels if str is visual string.\n" \
    "\n" \
    "For best results this function should be run on a whole paragraph, not\n" \
    "lines; but feel free to do otherwise if you know what you are doing.\n" \
    "\n" \
    "Returns a 4-tuple, (new_string, new_positions_to_this, new_position_from_this_list, new_embedding_levels)"
    # Original header file also says “Deprecated. Use fribidi_remove_special_chars instead.”
    # But I cannot find this function anywhere...
    c_string = str_to_chars(string)
    if positions_to_this != None :
        c_positions_to_this = seq_to_ct(positions_to_this, FRIBIDI.StrIndex)
    else :
        c_positions_to_this = None
    #end if
    if position_from_this_list != None :
        c_position_from_this_list = seq_to_ct(position_from_this_list, FRIBIDI.StrIndex)
    else :
        c_position_from_this_list = None
    #end if
    if embedding_levels != None :
        c_embedding_levels = seq_to_ct(embedding_levels, FRIBIDI.StrIndex)
    else :
        c_embedding_levels = None
    #end if
    new_str_len = fribidi.fribidi_remove_bidi_marks \
        (c_string, len(string), c_positions_to_this, c_position_from_this_list, c_embedding_levels)
    if new_str_len < 0 :
        raise RuntimeError("fribidi_remove_bidi_marks returned negative length")
          # out of memory?
    #end if
    result = [chars_to_str(c_string[:new_str_len]), None, None, None]
    if positions_to_this != None :
        result[1] = tuple(c_positions_to_this)
    #end if
    if position_from_this_list != None :
        result[2] = tuple(c_position_from_this_list[:new_str_len])
          # result of array seems to be unchanged and basically meaningless now
    #end if
    if embedding_levels != None :
        result[3] = tuple(c_embedding_levels)
    #end if
    return \
        tuple(result)
#end remove_bidi_marks

def log2vis(string, pbase_dir, want_positions_L_to_V = False, want_positions_V_to_L = False, want_embedding_levels = False) :
    "converts the logical input string to the visual output\n" \
    "strings as specified by the Unicode Bidirectional Algorithm. As a side\n" \
    "effect it also optionally generates mapping lists between the two strings, and\n" \
    "the list of embedding levels as defined by the algorithm.\n" \
    "\n" \
    "This function is obsolete because it only handles one-line paragraphs.\n" \
    "Please consider using other functions instead. Deprecated.\n" \
    "\n" \
    "Returns a 6-tuple: (Maximum level found in this line plus one, resolved paragraph base direction, visual string, optional positions_L_to_V, optional positions_V_to_L, optional embedding_levels)."
    strlen = len(string)
    c_string = str_to_chars(string)
    c_vis_string = (strlen * FRIBIDI.Char)()
    c_pbase_dir = FRIBIDI.ParType(pbase_dir)
    c_positions_L_to_V = None
    c_positions_V_to_L = None
    c_embedding_levels = None
    if want_positions_L_to_V :
        c_positions_L_to_V = (strlen * FRIBIDI.StrIndex)()
    #end if
    if want_positions_V_to_L :
        c_positions_V_to_L = (strlen * FRIBIDI.StrIndex)()
    #end if
    if want_embedding_levels :
        c_embedding_levels = (strlen * FRIBIDI.StrIndex)()
    #end if
    max_level = fribidi.fribidi_log2vis(c_string, strlen, ct.byref(c_pbase_dir), c_vis_string, c_positions_L_to_V, c_positions_V_to_L, c_embedding_levels)
    if max_level == 0 :
        raise RuntimeError("fribidi_log2vis returned 0")
          # out of memory?
    #end if
    result = [max_level, c_pbase_dir.value, chars_to_str(c_vis_string), None, None, None]
    if want_positions_L_to_V :
        result[3] = tuple(c_positions_L_to_V)
    #end if
    if want_positions_V_to_L :
        result[4] = tuple(c_positions_V_to_L)
    #end if
    if want_embedding_levels :
        result[5] = tuple(c_embedding_levels)
    #end if
    return \
        tuple(result)
#end log2vis

#+
# Convenience routines
#-

def each_embedding_run(vis_line, embedding_levels, vis_order) :
    "Generator function which yields in turn each contiguous run of the string" \
    " vis_line (previously reordered by FriBidi) which has the same embedding" \
    " level. Each result is a 4-tuple:\n" \
    "\n" \
    "     (substr, startindex, endindex, embedding_level)\n" \
    "\n" \
    "where substr is vis_line[startindex:endindex] and embedding_level is the" \
    " corresponding embedding level for the entire substring. The segments" \
    " are always returned in visual order (for rendering in turn left-to-right)," \
    " and internally in visual order if vis_order, else they are internally" \
    " rearranged to logical order (which is what HarfBuzz wants for shaping," \
    " for example)."
    assert len(vis_line) == len(embedding_levels)
    pos1 = pos2 = 0
    prev_level = None
    while True :
        if pos2 == len(vis_line) :
            cur_level = None
            if prev_level == None :
                break # zero-length string
        else :
            cur_level = embedding_levels[pos2]
            if prev_level == None :
                assert pos2 == 0
                prev_level = cur_level
            #end if
        #end if
        if cur_level != prev_level :
            substr = vis_line[pos1:pos2]
            if not vis_order and FRIBIDI.LEVEL_IS_RTL(prev_level) :
                # undo FriBidi reordering at intra-run level
                substr = "".join(reversed(substr))
            #end if
            yield (substr, pos1, pos2, prev_level)
            if pos2 == len(vis_line) :
                break
            prev_level = cur_level
            pos1 = pos2
        else :
            pos2 += 1
        #end if
    #end while
#end each_embedding_run

class ReorderLine :
    "convenience wrapper for a common use case: reorder a line of text and" \
    " extract the embedding runs. Instantiate with the text line to be processed" \
    " and the base direction (FRIBIDI.PAR_LTR or FRIBIDI.PAR_RTL). Then call the" \
    " each_embedding_run() method to iterate over the embedding runs."

    def __init__(self, text_line, base_dir, flags = FRIBIDI.FLAGS_DEFAULT) :
        self.text_line = text_line
        self.bidi_types = get_bidi_types(text_line)
        self.base_dir, self.embedding_levels = \
            get_par_embedding_levels(self.bidi_types, base_dir)[1:]
        self.vis_line, self.map = reorder_line \
          (
            flags = flags,
            bidi_types = self.bidi_types,
            line_offset = 0,
            base_dir = self.base_dir,
            embedding_levels = self.embedding_levels,
            logical_str = text_line,
            map = Reordering.identity(text_line)
          )[1:]
        self.vis_bidi_types = self.map.apply(self.bidi_types)
        self.vis_embedding_levels = self.map.apply(self.embedding_levels)
    #end __init__

    def each_embedding_run(self, vis_order) :
        "generator function which yields in turn each contiguous run of the string line" \
        " which has the same embedding level. Each result is a 4-tuple:\n" \
        "\n" \
        "     (substr, startindex, endindex, embedding_level)\n" \
        "\n" \
        "where substr is line[startindex:endindex] and embedding_level is the corresponding" \
        " embedding level for the entire substring."
        return \
            each_embedding_run(self.vis_line, self.vis_embedding_levels, vis_order)
    #end each_embedding_run

#end ReorderLine
