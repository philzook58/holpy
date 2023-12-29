(set-info :smt-lib-version 2.6)
(set-logic QF_UFBV)
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
(declare-fun f ((_ BitVec 32)) (_ BitVec 32))
(assert (= (f (bvadd a b)) c))
(assert (= (f (bvadd b a)) d))
(assert (not (= c d)))
(check-sat)
(exit)