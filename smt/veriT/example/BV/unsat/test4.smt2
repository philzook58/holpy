(set-info :smt-lib-version 2.6)
(set-logic QF_BV)
(set-info :status unsat)
(set-info :category "industrial")
(set-info :source |
  Generated using using the Low-Level Bounded Model Checker LLBMC.
  C files used in the paper: Florian Merz, Stephan Falke, Carsten Sinz: LLBMC: Bounded Model Checking of C and C++ Programs Using a Compiler IR. VSTTE 2012: 146-161
|)
(declare-fun a () (_ BitVec 32))
(declare-fun b () (_ BitVec 32))
(declare-fun c () (_ BitVec 32))
(declare-fun d () (_ BitVec 32))
(declare-fun e () (_ BitVec 32))
(assert (or (= d (bvadd a c)) (= d (bvadd b c))))
(assert (not (= (bvadd c (bvadd a b)) (bvadd d b))))
(assert (not (= (bvadd c (bvadd a b)) (bvadd d a))))
(check-sat)
(exit)
