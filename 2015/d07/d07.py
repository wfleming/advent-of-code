#!/usr/bin/env python

from sys import argv
import re
from functools import cached_property

class Gate:
    MASK = 0xffff

    def __init__(self, op, lop, rop, out):
        self.op = op
        self.lop = lop
        self.rop = rop
        self.out = out

    @cached_property
    def signal(self):
        if self.op == "NOT":
            return (~ self.rop_sig) & self.MASK
        elif self.op == "LSHIFT":
            return (self.lop_sig << self.rop_sig) & self.MASK
        elif self.op == "RSHIFT":
            return (self.lop_sig >> self.rop_sig) & self.MASK
        elif self.op == "AND":
            return (self.lop_sig & self.rop_sig)
        elif self.op == "OR":
            return (self.lop_sig | self.rop_sig)
        else:
            raise Exception(f"Invalid op {self.op} in circuit")

    @property
    def lop_sig(self):
        return self.get_signal(self.lop)

    @property
    def rop_sig(self):
        return self.get_signal(self.rop)

    def get_signal(self, s):
        if type(s) == int:
            return s
        else:
            return s.signal

    def __str__(self):
        rop_str = str(self.rop) if type(self.rop) == int else self.rop.name
        if self.op == "NOT":
            return f"{self.op} {rop_str} -> {self.out.name}"
        else:
            lop_str = str(self.lop) if type(self.lop) == int else self.lop.name
            return f"{lop_str} {self.op} {rop_str} -> {self.out.name}"

    def __repr__(self):
        return str(self)

class Wire:
    def __init__(self, name, incoming):
        self.name = name
        # could be int signal, another wire, or a gate
        self.incoming = incoming

    @cached_property
    def signal(self):
        if type(self.incoming) == int:
            return self.incoming
        else:
            return self.incoming.signal

    def __str__(self):
        return f"({self.name}, {self.signal})"

    def __repr__(self):
        return str(self)

class Circuit:
    WIRE_PAT = re.compile("(\w+) -> (\w+)")
    GATE_PAT = re.compile("(\w+) (AND|OR|LSHIFT|RSHIFT) (\w+) -> (\w+)")
    NOT_GATE_PAT = re.compile("NOT (\w+) -> (\w+)")
    DIGITS_PAT = re.compile("^(\d+)$")

    def __init__(self, wires, gates):
        self.wires = wires
        self.gates = gates

    @classmethod
    def parse(cls, filename):
        c = Circuit([], [])
        with open(filename, 'r') as fh:
            for l in fh.readlines():
                if m := cls.WIRE_PAT.match(l):
                    w = c.get_wire_or_signal(m[2])
                    w.incoming = c.get_wire_or_signal(m[1])
                elif m := cls.GATE_PAT.match(l):
                    out_wire = c.get_wire_or_signal(m[4])
                    gate = Gate(
                        op=m[2],
                        lop=c.get_wire_or_signal(m[1]),
                        rop=c.get_wire_or_signal(m[3]),
                        out=out_wire)
                    c.gates.append(gate)
                    out_wire.incoming = gate
                elif m := cls.NOT_GATE_PAT.match(l):
                    out_wire = c.get_wire_or_signal(m[2])
                    gate = Gate(
                        op="NOT",
                        lop=None,
                        rop=c.get_wire_or_signal(m[1]),
                        out=out_wire)
                    c.gates.append(gate)
                    out_wire.incoming = gate
                else:
                    raise Exception(f"couldn't parse line '{l}'")
        return c

    def get_wire_or_signal(self, name):
        if type(name) == int:
            return name
        elif self.DIGITS_PAT.match(name):
            return int(name)

        for w in self.wires:
            if w.name == name:
                return w
        w = Wire(name, 0)
        self.wires.append(w)
        return w

if __name__ == "__main__":
    c1 = Circuit.parse(argv[1])
    print(f"p1: circuit wires a={c1.get_wire_or_signal('a')}")

    c2 = Circuit.parse(argv[1])
    c2.get_wire_or_signal("b").incoming = c1.get_wire_or_signal("a")
    print(f"p1: circuit wires a={c2.get_wire_or_signal('a')}")
