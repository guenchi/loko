#!/usr/bin/env python
# -*- mode: python; coding: utf-8 -*-
# Copyright © 2018, 2019 Göran Weinholt <goran@weinholt.se>
# SPDX-License-Identifier: MIT

# Search for or prove Loko's type tagging and its properties.

from __future__ import print_function
from z3 import *
set_param(proof=True)

tag_fixnum     = BitVec('tag-fixnum', 64)
tag_box        = BitVec('tag-box', 64)
tag_pair       = BitVec('tag-pair', 64)
tag_procedure  = BitVec('tag-procedure', 64)
tag_string     = BitVec('tag-string', 64)
tag_vector     = BitVec('tag-vector', 64)
tag_bytevector = BitVec('tag-bytevector', 64)
tag_immsym     = BitVec('tag-immsym', 64)
tag_char       = BitVec('tag-char', 64)
tag_boolean    = BitVec('tag-boolean', 64)
tag_kill_mark  = BitVec('tag-kill-mark', 64)
tag_void       = BitVec('tag-void', 64)
tag_flonum     = BitVec('tag-flonum', 64)
tag_seek_mark  = BitVec('tag-seek-mark', 64)
tag_singular   = BitVec('tag-singular', 64)
tag_box_header = BitVec('tag-box-header', 64)  # first word in a box

tags = (
    tag_fixnum, tag_box, tag_pair, tag_procedure, tag_string, tag_vector, tag_bytevector,
    tag_immsym, tag_char, tag_boolean, tag_kill_mark, tag_void, tag_flonum, tag_seek_mark,
    tag_singular, tag_box_header
)

mask_fixnum     = BitVecVal(0b00000111, 64)
mask_box        = BitVecVal(0b00000111, 64)
mask_pair       = BitVecVal(0b00000111, 64)
mask_procedure  = BitVecVal(0b00000111, 64)
mask_string     = BitVecVal(0b00000111, 64)
mask_vector     = BitVecVal(0b00000111, 64)
mask_bytevector = BitVecVal(0b00000111, 64)
mask_immsym     = BitVecVal(0b00001111, 64)
mask_char       = BitVec('mask-char', 64)
mask_boolean    = BitVec('mask-boolean', 64)
mask_kill_mark  = BitVec('mask-kill-mark', 64)
mask_void       = BitVec('mask-void', 64)
mask_flonum     = BitVec('mask-flonum', 64)
mask_seek_mark  = BitVec('mask-seek-mark', 64)
mask_singular   = BitVec('mask-singular', 64)
mask_box_header = BitVec('mask-box-header', 64)

masks = (
    mask_fixnum, mask_box, mask_pair, mask_procedure, mask_string, mask_vector, mask_bytevector,
    mask_immsym, mask_char, mask_boolean, mask_kill_mark, mask_void, mask_flonum, mask_seek_mark,
    mask_singular, mask_box_header
)

# The first word in a box may be a box header. It has a type, length
# an optional value and a boolean that's true if the object contains
# references.

btag_symbol    = BitVec('btag-symbol', 64)
btag_bignum    = BitVec('btag-bignum', 64)
btag_ratnum    = BitVec('btag-ratnum', 64)
btag_pcompnum  = BitVec('btag-pcompnum', 64)
btag_rcompnum  = BitVec('btag-rcompnum', 64)
btag_rtd       = BitVec('btag-rtd', 64)
btag_rcd       = BitVec('btag-rcd', 64)

btags = (
    btag_symbol,
    btag_bignum,
    btag_ratnum,
    btag_pcompnum,
    btag_rcompnum,
    btag_rtd,
    btag_rcd,
)

mask_boxhdr_length = BitVec('mask-boxhdr-length', 64)
mask_boxhdr_refs_p = BitVec('mask-boxhdr-refs-p', 64)
mask_boxhdr_type   = BitVec('mask-boxhdr-type', 64)
mask_boxhdr_value  = BitVec('mask-boxhdr-value', 64)

# Setup the solver with the tags and masks that are currently in use
s = Solver()
s.add(tag_fixnum         == 0b000)
s.add(tag_box            == 0b001)
s.add(tag_pair           == 0b010)
s.add(tag_procedure      == 0b011)
s.add(tag_string         == 0b100)
s.add(tag_vector         == 0b110)
s.add(tag_bytevector     == 0b101)
s.add(tag_immsym        == 0b0111)
s.add(tag_char      == 0b00011111)
s.add(tag_boolean   == 0b10001111)
s.add(tag_flonum    == 0b01001111)
s.add(tag_singular  == 0b00101111)
# TODO: the gc wants the non-immediate tags to be
# (wraparound-)continuous because it makes it easier to check if
# something is an immediate

s.add(tag_kill_mark      == 0b100000001111)
s.add(tag_void       == 0b0000000000001111)
s.add(tag_seek_mark  == 0b1000000000001111 | 1<<63)
s.add(tag_box_header == 0b0100000000001111)

s.add(btag_bignum  == 0x01000000)
s.add(btag_ratnum  == 0x02000000)
s.add(btag_symbol  == 0x03000000)

# These are typechecked with byte/word registers
s.add(mask_char         == 0xff)
s.add(mask_flonum       == 0xff)
s.add(mask_boolean      == 0xff)
s.add(mask_kill_mark   == 0xfff)
s.add(mask_void       == 0xffff)
s.add(mask_seek_mark  == 0xffff | 1<<63)
s.add(mask_box_header == 0xffff)

s.add(mask_boxhdr_length == 0xffffffff00000000)
s.add(mask_boxhdr_refs_p == 0x0000000080000000)
s.add(mask_boxhdr_type   == 0x000000007f000000)
s.add(mask_boxhdr_value  == 0x0000000000ff0000)

# All immediates, except fixnums, have the same low three bits.
imm_tag  = BitVecVal(0b111, 64)
imm_mask = BitVecVal(0b111, 64)
s.add([(tag & imm_mask) == imm_tag for tag in
       (tag_immsym, tag_char, tag_boolean, tag_kill_mark,
        tag_void, tag_flonum, tag_seek_mark,
        tag_singular, tag_box_header)])

# Booleans can be created with setcc ah
s.add((mask_boolean & 0xff00) == 0)

# char->integer is a shift
s.add(Or((tag_char & 0b11100000) == 0,
         (tag_char & 0b11110000) == 0))

# Some size constraints for smaller instruction codings.
s.add([And(mask > 0, mask <= 0xffff) for mask in masks if mask is not mask_seek_mark])
# s.add(mask_char < 256)
# s.add(mask_flonum < 256)

# # Box headers have fields where there are no type bits
# s.add([(tag & (mask_boxhdr_length | mask_boxhdr_refs_p | mask_boxhdr_value)) == 0
#        for tag in btags])

# Tags fit inside their bitmasks
s.add([(tag & mask) == tag for (tag, mask) in zip(tags, masks)])
s.add([(tag & mask_boxhdr_type) == tag for tag in btags])

# Tags are distinct
s.add(Distinct(tags))
s.add(Distinct(btags))

def valid_pattern(x):
    return Or([((x & mask) == tag) for (mask, tag) in zip(masks, tags)])

# If a type predicate is true for x then all other type predicates return false.
x = BitVec('x', 64)
for i, (mask1, tag1) in enumerate(zip(masks, tags)):
    for j, (mask2, tag2) in enumerate(zip(masks, tags)):
        if i != j:
            s.add(ForAll([x],
                         # Either the bit pattern is unused or the
                         # type predicate is unique.
                         Or(Not(valid_pattern(x)),
                            Implies((x & mask1) == tag1,
                                    (x & mask2) != tag2))))

# Helpers for dual typechecks with bitwise IOR or AND.
def dual_IOR_typecheck(x, y, tag, mask, dual):
    # return And((x & mask) == tag, (y & mask) == tag) == (((x | y) & mask) == dual)
    return And(Implies(And((x & mask) == tag,
                           (y & mask) == tag),
                       ((x | y) & mask) == dual),
               Implies(Or((x & mask) != tag,
                          (y & mask) != tag),
                       ((x | y) & mask) != dual))

def dual_AND_typecheck(x, y, tag, mask, dual):
    # return And((x & mask) == tag, (y & mask) == tag) == (((x & y) & mask) == dual)
    return And(Implies(And((x & mask) == tag,
                           (y & mask) == tag),
                       ((x & y) & mask) == dual),
               Implies(Or((x & mask) != tag,
                          (y & mask) != tag),
                       ((x & y) & mask) != dual))

# Type check two fixnums simultaneously.
fxx = BitVec('fxx', 64)
fxy = BitVec('fxy', 64)
dual_fixnum  = BitVec('dual-fixnum', 64)
s.add(ForAll([fxx, fxy],
             Implies(And(valid_pattern(fxx), valid_pattern(fxy)),
                     dual_IOR_typecheck(fxx, fxy, tag_fixnum, mask_fixnum, dual_fixnum))))

# Two flonums can be typechecked simultaneously.
flx = BitVec('flx', 64)
fly = BitVec('fly', 64)
dual_flonum  = BitVec('dual-flonum', 64)
s.add(ForAll([flx, fly],
             Implies(And(valid_pattern(flx), valid_pattern(fly)),
                     dual_AND_typecheck(flx, fly, tag_flonum, mask_flonum, dual_flonum))))

# Two chars can be typechecked simultaneously.
chx = BitVec('chx', 64)
chy = BitVec('chy', 64)
dual_char  = BitVec('dual-char', 64)
s.add(ForAll([chx, chy],
             Implies(And(valid_pattern(chx), valid_pattern(chy)),
                     dual_AND_typecheck(chx, chy, tag_char, mask_char, dual_char))))

# Two booleans can be typechecked simultaneously.
boolx = BitVec('boolx', 64)
booly = BitVec('booly', 64)
dual_boolean  = BitVec('dual-boolean', 64)
s.add(ForAll([boolx, booly],
             Implies(And(valid_pattern(boolx), valid_pattern(booly)),
                     dual_AND_typecheck(boolx, booly, tag_boolean, mask_boolean, dual_boolean))))

# print(s.sexpr())
print('Searching for a solution...')
if repr(s.check()) == 'unsat':
    # Output is less than useful.
    print(s.proof())
else:
    m = s.model()
    print(m.sexpr())
