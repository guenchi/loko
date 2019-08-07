# -*- mode: python; coding: utf-8 -*-
# Copyright © 2019 Göran Weinholt
# SPDX-License-Identifier: AGPL-3.0-or-later

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
"""GDB support for debugging Loko

Sadly the Guile support in gdb was bit rotten when this was written.

"""

import gdb
import re

# Matches typetags.py and (loko arch amd64 objects)
tag_fixnum = 0b000
tag_box = 0b001
tag_pair = 0b010
tag_procedure = 0b011
tag_string = 0b100
tag_vector = 0b110
tag_bytevector = 0b101
tag_immsym = 0b0111
tag_char = 0b00011111
tag_boolean = 0b10001111
tag_flonum = 0b01001111
tag_singular = 0b00101111

tag_kill_mark = 0b100000001111
tag_void = 0b0000000000001111
tag_seek_mark = 0b1000000000001111 | 1<<63
tag_box_header = 0b0100000000001111

btag_bignum = 0x01000000
btag_ratnum = 0x02000000
btag_symbol = 0x03000000

mask_char  = 0xff
mask_flonum = 0xff
mask_boolean = 0xff
mask_kill_mark = 0xfff
mask_void = 0xffff
mask_seek_mark = 0xffff | 1<<63
mask_box_header = 0xffff

mask_boxhdr_length = 0xffffffff00000000
mask_boxhdr_refs_p = 0x0000000080000000
mask_boxhdr_type = 0x000000007f000000
mask_boxhdr_value = 0x0000000000ff0000

shift_fixnum = 3
shift_char = 8
shift_boolean = 8
shift_singular = 8

value_false = tag_boolean

def deref(val, idx):
    """Dereference a non-immediate Scheme object."""
    val = val & ~7
    val_ptr = val.cast(val.type.pointer())
    val_ptr += idx
    value = val_ptr.dereference()
    return value

class LokoPrinter(object):
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "#<unknown %x>" % int(self.val)


class LokoFixnumPrinter(LokoPrinter):
    def to_string(self):
        return "%d" % (self.val >> 3)


class LokoPairPrinter(LokoPrinter):
    def to_string(self):
        car = deref(self.val, 0)
        cdr = deref(self.val, 1)
        return "(%s . %s)" % (car, cdr)


class LokoVectorPrinter(LokoPrinter):
    def children(self):
        length = deref(self.val, 0) >> shift_fixnum
        for i in range(length):
            yield str(i), deref(self.val, i + 1)

    def to_string(self):
        return "vector"

    def display_hint(self):
        return 'array'


class LokoBytevectorPrinter(LokoPrinter):
    def children(self):
        length = deref(self.val, 0) >> shift_fixnum
        val = (self.val & ~7) + 16
        val_ptr = val.cast(gdb.lookup_type('unsigned char').pointer())
        for i in range(length):
            value = (val_ptr + i).dereference()
            yield str(i), value

    def to_string(self):
        return "vu8"

    def display_hint(self):
        return 'array'


class LokoBoxPrinter(object):
    def __init__(self, val, boxheader):
        self.val = val
        self.boxheader = boxheader

    def to_string(self):
        return "#<%s>" % self.boxheader


class LokoGensymPrinter(LokoBoxPrinter):
    def to_string(self):
        return '#<gensym>'

    def children(self):
        prefix = deref(self.val, 1)
        yield '0', 'prefix'
        if prefix == value_false:
            yield '1', prefix
        else:
            bv = bytes(int(ch) for _, ch in LokoBytevectorPrinter(prefix).children())
            yield '1', bv.decode('utf-8')

        unique = deref(self.val, 2)
        yield '2', 'unique'
        if unique == value_false:
            yield '3', unique
        else:
            bv = bytes(int(ch) for _, ch in LokoBytevectorPrinter(unique).children())
            yield '3', bv.decode('utf-8')

        yield '4', 'value'
        yield '5', deref(self.val, 3)

    def display_hint(self):
        return 'map'


class LokoSymbolPrinter(LokoBoxPrinter):
    def to_string(self):
        name = deref(self.val, 1)
        namebv = bytes(int(ch) for _, ch in LokoBytevectorPrinter(name).children())
        return namebv.decode('utf-8')


class LokoRecordPrinter(LokoBoxPrinter):
    def __init__(self, val, boxheader):
        super(LokoRecordPrinter, self).__init__(val, boxheader)
        self.flags_and_len = deref(self.boxheader, 2)
        self.name = deref(self.boxheader, 3)
        self.parent = deref(self.boxheader, 4)
        self.uid = deref(self.boxheader, 5)
        self.names = deref(self.boxheader, 6)
        self.mutable = deref(self.boxheader, 7)

    def to_string(self):
        return '#[%s]' % (self.name or self.uid)

    def display_hint(self):
        return 'map'

    def children(self):
        i = 1
        def recurse(rtd):
            nonlocal i
            if rtd == value_false:
                return
            parent = deref(rtd, 4)
            names = LokoVectorPrinter(deref(rtd, 6)).children()
            yield from recurse(parent)
            for _, name in names:
                yield "nam%d" % i, str(name)
                yield "val%d" % i, deref(self.val, i)
                i += 1

        yield from recurse(self.boxheader)


class LokoProcedurePrinter(LokoPrinter):
    def __init__(self, val):
        self.val = val
        self.entry = deref(self.val, 0)
        self.info = deref(self.val, 1)
        self.freevars = deref(self.info, 2)
        self.name = deref(self.info, 3)
        self.source = deref(self.info, 4)

    def to_string(self):
        if self.freevars != 0:
            return "#<closure>"
        else:
            return "#<procedure>"

    def children(self):
        yield 'name', self.name
        # FIXME: This should be returned as a linker symbol
        yield 'entry', hex(self.entry)
        yield 'source', self.source
        # FIXME: get all free variables


class LokoImmsymPrinter(LokoPrinter):
    def to_string(self):
        val = int(self.val) >> 4
        chars = []
        while val:
            idx = (val & ((1 << 5) - 1)) - 1
            val >>= 5
            if val > 0:
                chars.append("abcdefghijklmnopqrstuvwxyz-/<=>"[idx])
            else:
                chars.append("acdefghklmnopqrstvxy!*+-/08<=>?"[idx])

        return "".join(chars)


class LokoStringPrinter(LokoPrinter):
    def to_string(self):
        def string_ref(val, idx):
            val = (val & ~7) + 8
            val_ptr = val.cast(gdb.lookup_type('unsigned int').pointer())
            val_ptr += idx
            value = val_ptr.dereference()
            if value & 0xff == 0x1f:
                return chr(value >> shift_char)
            return '\U0000FFFD'

        chars = ''
        length = deref(self.val, 0) >> shift_fixnum
        for i in range(length):
            chars += string_ref(self.val, i)

        return chars

    def display_hint(self):
        return 'string'


class LokoSingularPrinter(LokoPrinter):
    def to_string(self):
        return self.val


def loko_lookup_function(val):
    if str(val.type) == 'int64_t':
        tag3 = int(val) & 0b111
        if tag3 == tag_fixnum:
            return LokoFixnumPrinter(val)

        if tag3 == tag_pair:
            return LokoPairPrinter(val)
        if tag3 == tag_procedure:
            return LokoProcedurePrinter(val)
        if tag3 == tag_string:
            return LokoStringPrinter(val)
        if tag3 == tag_vector:
            return LokoVectorPrinter(val)
        if tag3 == tag_bytevector:
            return LokoBytevectorPrinter(val)

        if tag3 == tag_box:
            boxheader = deref(val, 0)
            if int(boxheader) & 0b1111 == tag_immsym:
                # FIXME: Obsolete
                oldtype = LokoImmsymPrinter(boxheader).to_string()
                # if oldtype == 'symbol':
                #     return LokoSymbolPrinter(val, boxheader)
            if int(boxheader) & 0b111 == tag_box:
                boxheader2 = deref(boxheader, 0)
                oldtype2 = LokoImmsymPrinter(boxheader2).to_string()
                if oldtype2 == 'rtd':
                    return LokoRecordPrinter(val, boxheader)
            if int(boxheader) & mask_box_header == tag_box_header:
                if int(boxheader) & mask_boxhdr_type == btag_symbol:
                    if int(boxheader) & mask_boxhdr_value:
                        return LokoGensymPrinter(val, boxheader)
                    else:
                        return LokoSymbolPrinter(val, boxheader)

            return LokoBoxPrinter(val, boxheader)

        tag4 = int(val) & 0b1111
        if tag4 == tag_immsym:
            return LokoImmsymPrinter(val)

        tag8 = int(val) & 0xff
        if tag8 == tag_singular:
            stype = int(val) >> shift_singular
            if stype == 0:
                return LokoSingularPrinter('()')
            if stype == 1:
                return LokoSingularPrinter('#!eof')
            if stype == 2:
                return LokoSingularPrinter('#!moved')
        if tag8 == tag_boolean:
            value = int(val) >> shift_boolean
            if value == 0:
                return LokoSingularPrinter('#f')
            if value == 1:
                return LokoSingularPrinter('#t')

        return LokoPrinter(val)

    return None

# Frame unwinding

from gdb.unwinder import Unwinder

class FrameId(object):
    def __init__(self, sp, pc):
        self.sp = sp
        self.pc = pc


class LokoUnwinder(Unwinder):
    def __init__(self):
        super(LokoUnwinder, self).__init__("loko")
        self.have_warned = False

    def __call__(self, pending_frame):
        sp = pending_frame.read_register('rsp')
        pc = pending_frame.read_register('rip')
        unwind_info = pending_frame.create_unwind_info(FrameId(sp, pc))

        # FIXME: gdb is not finding the table...
        table = gdb.lookup_global_symbol('bootstrap_unwind_table')
        if table is None:
            if not self.have_warned:
                print("Warning: loko-gdb.py is using a hardcoded value")
                self.have_warned = True
            table = gdb.Value(0x472000).cast(gdb.lookup_type('unsigned long').pointer())
        #print('table', table)

        # Skip over the locals (unwind)
        table_size = table.dereference() >> shift_fixnum
        for i in range(table_size / 3):
            first_label = (table + 1 + i * 3).dereference() >> 3
            last_label = (table + 2 + i * 3).dereference() >> 3
            frame_size = (table + 3 + i * 3).dereference() >> 3
            if pc >= first_label and pc < last_label:
                # Skip over the return address and the locals
                previous_sp = sp + 8 * (frame_size + 1)
                break
        else:
            previous_sp = sp + 8
        unwind_info.add_saved_register('rsp', previous_sp)

        # Return address
        sp_ptr = (previous_sp-8).cast(gdb.lookup_type('unsigned long').pointer())
        previous_rip = sp_ptr.dereference()
        unwind_info.add_saved_register('rip', previous_rip)

        return unwind_info


gdb.pretty_printers.append(loko_lookup_function)
gdb.unwinder.register_unwinder(None, LokoUnwinder())
