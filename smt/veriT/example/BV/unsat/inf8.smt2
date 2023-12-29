(set-info :smt-lib-version 2.6)
(set-logic QF_BV)
(set-info :status unsat)
(set-info :category "industrial")
(set-info :source |
  Generated using using the Low-Level Bounded Model Checker LLBMC.
  C files used in the paper: Florian Merz, Stephan Falke, Carsten Sinz: LLBMC: Bounded Model Checking of C and C++ Programs Using a Compiler IR. VSTTE 2012: 146-161
|)
(declare-fun a_0x1dc6fc0 () (_ BitVec 32))
(declare-fun b_0x1dc5750 () (_ BitVec 32))
(declare-fun c_0x1ddc810 () (_ BitVec 32))
(declare-fun d_0x1ddc8f0 () (_ BitVec 32))
(assert
(let ((?x1 (_ bv0 1)))
(let ((?x2 (_ bv1 1)))
(let ((?x3 (_ bv0 32)))
(let ((?x4 (_ bv1 32)))
(let ((?x5 a_0x1dc6fc0))
(let ((?x6 b_0x1dc5750))
(let ((?x7 c_0x1ddc810))
(let ((?x8 d_0x1ddc8f0))
(let (($x9 (= ?x7 ?x3)))
(let (($x10 (distinct ?x7 ?x3)))
(let (($x11 (= ?x6 ?x3)))
(let (($x12 (and $x10 $x11)))
(let (($x13 (or $x12 $x9)))
(let (($x14 (distinct ?x6 ?x3)))
(let (($x15 (and $x10 $x14)))
(let (($x16 (or $x13 $x15)))
(let (($x17 (not $x13)))
(let (($x18 (and $x17 $x16)))
(let (($x19 (and $x9 $x14)))
(let (($x20 (and $x13 $x10)))
(let (($x21 (or $x19 $x20)))
(let (($x22 (and $x21 $x14)))
(let (($x23 (not $x22)))
(let (($x24 (or $x23 $x9)))
(let (($x25 (not $x24)))
(let (($x26 (and $x22 $x9)))
(let (($x27 (and $x21 $x11)))
(let (($x28 (or $x26 $x27)))
(let ((?x29 (ite $x26 ?x3 ?x4)))
(let ((?x30 (ite $x26 ?x4 ?x3)))
(let (($x31 (and $x9 $x11)))
(let (($x32 (= ?x5 ?x4)))
(let (($x33 (and $x31 $x32)))
(let (($x34 (= ?x8 ?x4)))
(let (($x35 (and $x33 $x34)))
(let ((?x36 (ite $x35 ?x4 ?x3)))
(let (($x37 (or $x28 $x31)))
(let ((?x38 (ite $x28 ?x29 ?x4)))
(let ((?x39 (ite $x28 ?x30 ?x4)))
(let ((?x40 (ite $x28 ?x3 ?x36)))
(let (($x41 (or $x18 $x37)))
(let ((?x42 (ite $x18 ?x3 ?x38)))
(let ((?x43 (ite $x18 ?x3 ?x39)))
(let ((?x44 (ite $x18 ?x3 ?x40)))
(let (($x45 (and $x13 $x41)))
(let (($x46 (= ?x44 ?x3)))
(let (($x47 (and $x45 $x46)))
(let (($x48 (= ?x42 ?x3)))
(let (($x49 (and $x47 $x48)))
(let (($x50 (distinct ?x43 ?x3)))
(let ((?x51 (ite $x50 (_ bv1 1) (_ bv0 1))))
(let ((?x52 (ite $x49 ?x51 ?x2)))
(let (($x53 (= ?x52 (_ bv1 1))))
(let (($x54 (not $x45)))
(let (($x55 (or $x54 $x53)))
(let (($x56 (not $x55)))
(let (($x57 (or $x25 $x56)))
(let (($x58 (and $x45 $x53)))
(let (($x59 (and $x17 $x41)))
(let (($x60 (or $x58 $x59)))
(let (($x61 (and $x13 $x60)))
(let (($x62 (= ?x43 ?x42)))
(let (($x63 (and $x61 $x62)))
(let (($x64 (and $x63 $x9)))
(let (($x65 (= ?x6 ?x3)))
(let ((?x66 (ite $x65 (_ bv1 1) (_ bv0 1))))
(let ((?x67 (ite $x64 ?x66 ?x1)))
(let (($x68 (= ?x67 (_ bv1 1))))
(let (($x69 (not $x63)))
(let (($x70 (or $x69 $x68)))
(let (($x71 (not $x70)))
(let (($x72 (or $x57 $x71)))
(let (($x73 (and $x63 $x68)))
(let (($x74 (distinct ?x43 ?x42)))
(let (($x75 (and $x61 $x74)))
(let (($x76 (or $x73 $x75)))
(let (($x77 (and $x17 $x60)))
(let (($x78 (or $x76 $x77)))
(let (($x79 (not $x18)))
(let (($x80 (and $x79 $x78)))
(let ((?x81 (ite $x13 ?x2 ?x1)))
(let ((?x82 (ite $x80 ?x81 ?x2)))
(let (($x83 (= ?x82 (_ bv1 1))))
(let (($x84 (not $x78)))
(let (($x85 (or $x84 $x83)))
(let (($x86 (not $x85)))
(let (($x87 (or $x72 $x86)))
$x87
)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
)
(check-sat)
(exit)