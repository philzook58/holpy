(set-info :smt-lib-version 2.6)
(set-logic QF_AUFLIA)
(set-info :source |
Benchmarks used in the followin paper:
Big proof engines as little proof engines: new results on rewrite-based satisfiability procedure
Alessandro Armando, Maria Paola Bonacina, Silvio Ranise, Stephan Schulz. 
PDPAR'05
http://www.ai.dist.unige.it/pdpar05/


|)
(set-info :category "crafted")
(set-info :status unsat)
(declare-fun a_1 () (Array Int Int))
(declare-fun a_10 () (Array Int Int))
(declare-fun a_3 () (Array Int Int))
(declare-fun a_5 () (Array Int Int))
(declare-fun a_6 () (Array Int Int))
(declare-fun a_8 () (Array Int Int))
(declare-fun e_0 () Int)
(declare-fun e_12 () Int)
(declare-fun e_2 () Int)
(declare-fun e_4 () Int)
(declare-fun e_7 () Int)
(declare-fun e_9 () Int)
(declare-fun i_11 () Int)
(declare-fun a1 () (Array Int Int))
(declare-fun i0 () Int)
(declare-fun i1 () Int)
(declare-fun i2 () Int)
(declare-fun sk ((Array Int Int) (Array Int Int)) Int)
(assert (= a_1 (store a1 i0 e_0)))
(assert (= a_10 (store a_8 i0 e_9)))
(assert (= a_3 (store a_1 i1 e_2)))
(assert (= a_5 (store a_3 i2 e_4)))
(assert (= a_6 (store a_5 i2 e_4)))
(assert (= a_8 (store a_6 i2 e_7)))
(assert (= e_0 (select a1 i1)))
(assert (= e_12 (select a_10 i_11)))
(assert (= e_2 (select a1 i0)))
(assert (= e_4 (select a_3 i2)))
(assert (= e_7 (select a_6 i0)))
(assert (= e_9 (select a_6 i2)))
(assert (= i_11 (sk a_10 a_10)))
(assert (not (= e_12 e_12)))
(check-sat)
(exit)