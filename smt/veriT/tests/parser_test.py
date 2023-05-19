import unittest
import time

from smt.veriT.proof_parser import parse_decl, proof_parser
from smt.veriT import interface, proof_rec
from smt.veriT import command
import os
from syntax.settings import settings
import sys
sys.setrecursionlimit(10000)

settings.unicode = False

smtlib_path = None

try:
    with open('smt/veriT/tests/smtlib_path.txt', 'r') as f:
        smtlib_path = f.readline().strip()
except FileNotFoundError:
    print("File smtlib_path.txt should be present in the smt/veriT/tests/ directory,")
    print("containing the path to the smtlib folder.")


def test_parse_step(verit_proof, ctx):
    parser = proof_parser(ctx)
    for s in verit_proof.replace("\r", "").split("\n"):
        if s == "unsat" or s == "":
            continue
        # print(s)
        step = parser.parse(s)
        if isinstance(step, command.Step) and step.rule_name == "lia_generic":
            print('lia_generic')

def test_file(filename, write_file=False, show_time=True, veriT_only=False, parse_assertion=False):
    """Test a single SMT file.
    
    filename : str - absolute path of the file.
    write_file : bool - whether to write proof to proof.txt.
    show_time : bool - whether to display time taken.
    veriT_only : bool - only check whether veriT returns unsat.
    
    """
    global smtlib_path
    if not smtlib_path:
        return

    if filename[-4:] != 'smt2':
        return

    abs_name = smtlib_path + filename
    print("Test file", abs_name)

    # if not interface.is_unsat(abs_name):
    #     print('sat / unknown / unsupported')
    #     return
    
    if parse_assertion:
        assts = proof_rec.get_assertions(abs_name)
    else:
        assts = set()

    # Solve
    start_time = time.perf_counter()
    verit_proof = interface.solve(abs_name)
    solve_time = time.perf_counter() - start_time
    print("Finished solve in", solve_time)
    if verit_proof is None:
        return

    if len(verit_proof) == 0:
        print('Empty proof')
        return
    if verit_proof == "unknown\r\n":
        print("unknown proof")
        return

    # All veriT jobs finished. Output file name. 
    print(repr(filename) + ',')

    # Optional: write to file
    if write_file:
        with open('proof.txt', 'w') as f:
            f.write(verit_proof)

    # Only check whether veriT finishes
    if veriT_only:
        return

    # Parse
    start_time = time.perf_counter()
    ctx = proof_rec.bind_var(abs_name)
    test_parse_step(verit_proof, ctx)
    parse_time = time.perf_counter() - start_time

    if show_time:
        print("Solve: %.3f. Parse: %.3f" % (solve_time, parse_time))

def test_path(path, write_file=False, show_time=False, veriT_only=False, parse_assertion=False):
    """Test a directory containing SMT files."""
    global smtlib_path
    if not smtlib_path:
        return

    abs_path = smtlib_path + path

    if not os.path.exists(abs_path):
        print("Directory %s not found." % path)
        return

    if os.path.isdir(abs_path):
        # Input is a directory
        sub_paths = [path + '/' + child for child in os.listdir(abs_path)]
        for sub_path in sub_paths:
            test_path(sub_path, write_file=write_file, show_time=show_time, veriT_only=veriT_only, parse_assertion=parse_assertion)
    else:
        # Input is a file
        test_file(path, write_file=write_file, show_time=show_time, veriT_only=veriT_only, parse_assertion=parse_assertion)


class ParserTest(unittest.TestCase): 
    def testParseQF_UF(self):
        test_paths = [
            'QF_UF/20170829-Rodin/smt1300175744189082250.smt2',
            'QF_UF/20170829-Rodin/smt1468783596909311386.smt2',
            'QF_UF/20170829-Rodin/smt2061505268723161891.smt2',
            'QF_UF/20170829-Rodin/smt2080745738819601301.smt2',
            'QF_UF/20170829-Rodin/smt2325451563592377472.smt2',
            'QF_UF/20170829-Rodin/smt249825283571301584.smt2',
            'QF_UF/20170829-Rodin/smt2598599073465845145.smt2',
            'QF_UF/20170829-Rodin/smt2970577543992530805.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_blocks.2.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_bridge.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_brp.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_bug-1_ab_cti_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_cache_coherence_three_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_cambridge.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_collision.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_counter_v_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_cyclic_scheduler.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_elevator.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_eq_sdp_v7_ab_cti_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_exit.1.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_extinction.2.prop1_ab_reg_max.smt2',
            'QF_UF/2018-Goel-hwbench/QF_UF_firewire_tree.1.prop1_ab_reg_max.smt2',
            'QF_UF/eq_diamond/eq_diamond1.smt2',
            'QF_UF/eq_diamond/eq_diamond10.smt2',
            "QF_UF/NEQ/NEQ004_size4.smt2",
            "QF_UF/NEQ/NEQ004_size5.smt2",
            "QF_UF/NEQ/NEQ006_size3.smt2",
            "QF_UF/PEQ/PEQ012_size3.smt2",
            "QF_UF/QG-classification/loops6/iso_icl103.smt2",
            "QF_UF/QG-classification/qg5/iso_icl054.smt2",
            'QF_UF/SEQ/SEQ004_size5.smt2',
            'QF_UF/SEQ/SEQ004_size6.smt2',
            'QF_UF/SEQ/SEQ005_size7.smt2',
            "QF_UF/TypeSafe/z3.1184131.smt2",
            "QF_UF/TypeSafe/z3.1184147.smt2",
            "QF_UF/TypeSafe/z3.1184163.smt2",
        ]

        for path in test_paths:
            test_path(path)

    def testParseQF_UFLRA(self):
        test_paths = [
            'QF_UFLRA/mathsat/RandomCoupled/pb_real_10_0200_10_14.smt2',
            'QF_UFLRA/mathsat/RandomCoupled/pb_real_20_0400_10_12.smt2',
            'QF_UFLRA/mathsat/RandomCoupled/pb_real_30_0600_10_13.smt2',
            'QF_UFLRA/mathsat/RandomDecoupled/pb_real_30_60_45_06.smt2',
            'QF_UFLRA/mathsat/RandomDecoupled/pb_real_40_80_60_01.smt2',
            'QF_UFLRA/mathsat/RandomDecoupled/pb_real_50_100_30_07.smt2',
            'QF_UFLRA/FFT/smtlib.624882.smt2',
            'QF_UFLRA/FFT/smtlib.624898.smt2',
            'QF_UFLRA/FFT/smtlib.624916.smt2',
            'QF_UFLRA/FFT/smtlib.626139.smt2',
            'QF_UFLRA/FFT/smtlib.626159.smt2',
            'QF_UFLRA/FFT/smtlib.626179.smt2',
        ]

        for path in test_paths:
            test_path(path)


    def testParseQF_UFLIA(self):
        test_paths = [
            'QF_UFLIA/mathsat/EufLaArithmetic/medium/medium5.smt2',
            'QF_UFLIA/mathsat/EufLaArithmetic/medium/medium6.smt2',
            'QF_UFLIA/mathsat/EufLaArithmetic/hard/hard4.smt2',
            'QF_UFLIA/mathsat/EufLaArithmetic/hard/hard5.smt2',
            'QF_UFLIA/mathsat/Hash/hash_uns_03_03.smt2',
            'QF_UFLIA/mathsat/Hash/hash_uns_03_04.smt2',
            'QF_UFLIA/mathsat/Wisa/xs-05-08-4-2-5-4.smt2',
            'QF_UFLIA/mathsat/Wisa/xs-05-12-1-4-2-1.smt2',
            'QF_UFLIA/mathsat/Wisa/xs-05-16-1-5-4-3.smt2',
            'QF_UFLIA/TwoSquares/smtlib.602033.smt2',
            'QF_UFLIA/TwoSquares/smtlib.602046.smt2',
            'QF_UFLIA/TwoSquares/smtlib.686126.smt2',
            'QF_UFLIA/TwoSquares/smtlib.769286.smt2',
            'QF_UFLIA/wisas/xs_7_12.smt2',
        ]

        for path in test_paths:
            test_path(path)


    def testParseUF(self):
        test_paths = [
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_16_59_39_090_1045351.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_17_00_12_337_1079814.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_17_00_49_980_1120402.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_52_18_634_983654.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_53_05_211_1033050.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_53_31_362_1064389.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_48_45_757_1043506.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_54_30_307_1349771.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_57_04_292_1481164.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_46_30_200_1001391.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_47_39_480_1078027.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_48_44_767_1147663.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_54_35_132_1014381.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_54_53_474_1036287.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_55_00_922_1043783.smt_in.smt2',
            'UF/20201221-induction-by-reflection-schoisswohl/reflectiveConjecture/currying-0.smt2',
            'UF/sledgehammer/Arrow_Order/smtlib.578262.smt2',
            'UF/sledgehammer/Arrow_Order/smtlib.617784.smt2',
            'UF/sledgehammer/Arrow_Order/smtlib.686801.smt2',
            'UF/sledgehammer/FFT/uf.549548.smt2',
            'UF/sledgehammer/FFT/uf.600765.smt2',
            'UF/sledgehammer/FFT/uf.626085.smt2',
            'UF/sledgehammer/Fundamental_Theorem_Algebra/uf.1025050.smt2',
            'UF/sledgehammer/Fundamental_Theorem_Algebra/uf.1061982.smt2',
            'UF/sledgehammer/Hoare/smtlib.1170876.smt2',
            'UF/sledgehammer/Hoare/uf.1008477.smt2',
            'UF/sledgehammer/Hoare/uf.1031408.smt2',
            'UF/sledgehammer/StrongNorm/uf.701666.smt2',
            "UF/sledgehammer/TypeSafe/smtlib.1267524.smt2",
            'UF/sledgehammer/TwoSquares//uf.680734.smt2',
            'UF/grasshopper/instantiated/concat_check_heap_access_23_4.smt2',
            'UF/grasshopper/instantiated/concat_invariant_18_4.smt2',
            'UF/grasshopper/instantiated/dl_filter_postcondition_of_dl_filter_41_1.smt2',
            'UF/grasshopper/uninstantiated/dl_filter_loop_invariant_40_3.smt2',
            'UF/grasshopper/uninstantiated/dl_filter_postcondition_of_dl_filter_41_1.smt2',
            'UF/grasshopper/uninstantiated/dl_insert_check_heap_access_16_4.smt2',
            'UF/misc/list1.smt2',
            'UF/misc/set10.smt2',
            'UF/misc/set11.smt2',
        ]

        for path in test_paths:
            test_path(path)

    def testParseUFLRA(self):
        test_paths = [
            'UFLRA/FFT/smtlib.620487.smt2',
            'UFLRA/FFT/smtlib.620535.smt2',
            'UFLRA/FFT/smtlib.623417.smt2',
            'UFLRA/FFT/smtlib.623474.smt2',
            'UFLRA/FFT/smtlib.623531.smt2',
            'UFLRA/FFT/smtlib.623597.smt2',
            'UFLRA/FFT/smtlib.627457.smt2',
            'UFLRA/FFT/smtlib.627531.smt2',
            'UFLRA/FFT/smtlib.627605.smt2',
            'UFLRA/FFT/smtlib.627688.smt2',
        ]

        for path in test_paths:
            test_path(path)


    def testParseUFLIA(self):
        test_paths = [
            'UFLIA/boogie/QuantifierVisibilityInvariant_C..ctor_System.Int32.smt2',
            'UFLIA/boogie/AdditiveMethods_AdditiveMethods..ctor.smt2',
            'UFLIA/boogie/AdditiveMethods_AdditiveMethods.M.smt2',
            'UFLIA/boogie/AdditiveMethods_AdditiveMethods.N.smt2',
            'UFLIA/boogie/Array_test3.MyClass.M0_test3.MyClass.array_notnull.smt2',
            'UFLIA/boogie/Array_test3.MyClass.M1_test3.MyClass.array.array_notnull.smt2',
            'UFLIA/boogie/Array_test3.MyClass.M2_test3.MyClass.array2_notnull.smt2',
            'UFLIA/check/bignum_quant.smt2',
            'UFLIA/misc/arr1.smt2',
            'UFLIA/misc/list3.smt2',
            'UFLIA/misc/list5.smt2',
            'UFLIA/RicartAgrawala/ricart-agrawala1.smt2',
            'UFLIA/sexpr/SES.Atom..ctor_System.String_notnull.smt2',
            'UFLIA/sexpr/SES.Atom.Write_System.IO.TextWriter_notnull.smt2',
            'UFLIA/simplify/javafe.ast.AmbiguousMethodInvocation.29.smt2',
            'UFLIA/simplify/javafe.ast.BinaryExpr.48.smt2',
            'UFLIA/simplify/javafe.ast.BlockStmt.50.smt2',
            'UFLIA/simplify2/front_end_suite/javafe.ast.AmbiguousMethodInvocation.001.smt2',
            'UFLIA/simplify2/front_end_suite/javafe.ast.AmbiguousVariableAccess.001.smt2',
            'UFLIA/simplify2/front_end_suite/javafe.ast.ArrayInit.001.smt2',
            'UFLIA/sledgehammer/Arrow_Order/smtlib.555057.smt2',
            'UFLIA/sledgehammer/Arrow_Order/smtlib.555849.smt2',
            'UFLIA/sledgehammer/Arrow_Order/smtlib.556254.smt2',
            'UFLIA/spec_sharp/textbook-DutchFlag.bpl.1.Partition.smt2',
            'UFLIA/spec_sharp/textbook-Find.bpl.1.Find.smt2',
            'UFLIA/spec_sharp/textbook-Find.bpl.2.Main.smt2',
            'UFLIA/tokeneer/admin/finishop-1.smt2',
            'UFLIA/tokeneer/admin/init-1.smt2',
            'UFLIA/tokeneer/admin/logon-1.smt2',
            'UFLIA/tokeneer/userentry/progress-4.smt2',
            'UFLIA/tptp/ARI604=1.smt2',
            'UFLIA/tptp/ARI612=1.smt2',
            'UFLIA/tptp/ARI615=1.smt2',
        ]

        for path in test_paths:
            test_path(path)

    def testParseQF_RDL(self):
        test_paths = [
            # 'QF_RDL/check/bignum_rdl2.smt2',
            # 'QF_RDL/sal/fischer3-mutex-1.smt2',
            'QF_RDL/sal/fischer3-mutex-10.smt2',
            'QF_RDL/sal/fischer6-mutex-2.smt2',
            'QF_RDL/sal/fischer6-mutex-3.smt2',
            'QF_RDL/sal/fischer6-mutex-4.smt2',
            'QF_RDL/scheduling/abz5_1000.smt2',
            'QF_RDL/scheduling/orb01_900.smt2',
            'QF_RDL/scheduling/orb05_800.smt2',
            'QF_RDL/skdmxa/skdmxa-3x3-5.smt2',
            'QF_RDL/skdmxa2/skdmxa-3x3-5.base.cvc.smt2',
        ]

        for path in test_paths:
            test_path(path)

    def testParseQF_AUFLIA(self):
        test_paths = [
            'QF_AUFLIA/20170829-Rodin/smt101358492275879472.smt2',
            'QF_AUFLIA/20170829-Rodin/smt1048206973303286471.smt2',
            'QF_AUFLIA/20170829-Rodin/smt1076382332286802622.smt2',
            'QF_AUFLIA/20170829-Rodin/smt9116345646566616227.smt2',
            'QF_AUFLIA/20170829-Rodin/smt957085527369554317.smt2',
            'QF_AUFLIA/20170829-Rodin/smt971450140125177067.smt2',
            'QF_AUFLIA/cvc/add4.smt2',
            'QF_AUFLIA/cvc/add5.smt2',
            'QF_AUFLIA/cvc/add6.smt2',
            'QF_AUFLIA/cvc/fb_var_33_6.smt2',
            'QF_AUFLIA/cvc/pp-invariant.smt2',
            'QF_AUFLIA/cvc/pp-pc-s2i.smt2',
            'QF_AUFLIA/cvc/read6.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_nf_ai_00002_001.cvc.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_nf_ai_00003_003.cvc.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_nf_ai_00008_003.cvc.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_sf_ai_00002_001.cvc.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_sf_ai_00003_003.cvc.smt2',
            'QF_AUFLIA/swap/swap_t1_pp_sf_ai_00008_003.cvc.smt2',
            'QF_AUFLIA/swap/swap_t3_pp_nf_ai_00005_001.cvc.smt2',
            'QF_AUFLIA/swap/swap_t3_pp_sf_ai_00005_001.cvc.smt2',
        ]

        for path in test_paths:
            test_path(path)
        
    def testParseAUFLIA(self):
        test_paths = [
            'AUFLIA/20170829-Rodin/smt1002232729905089644.smt2',
            'AUFLIA/20170829-Rodin/smt1005619206308662052.smt2',
            'AUFLIA/20170829-Rodin/smt1041855950602319657.smt2',
            'AUFLIA/20170829-Rodin/smt1056800823907916951.smt2',
            'AUFLIA/20170829-Rodin/smt1092100299585472351.smt2',
            'AUFLIA/20170829-Rodin/smt1119354235255245041.smt2',
            'AUFLIA/20170829-Rodin/smt121642227126350719.smt2',
            'AUFLIA/20170829-Rodin/smt1218456647866960210.smt2',
            'AUFLIA/20170829-Rodin/smt1238392701218460929.smt2',
            'AUFLIA/20170829-Rodin/smt1548357652033660278.smt2',
            'AUFLIA/20170829-Rodin/smt1551255090000261050.smt2',
            'AUFLIA/20170829-Rodin/smt1553283421285438203.smt2',
            'AUFLIA/20170829-Rodin/smt1638875703304617187.smt2',
            'AUFLIA/20170829-Rodin/smt1643313206004053741.smt2',
            'AUFLIA/20170829-Rodin/smt1643358356540277150.smt2',
            'AUFLIA/20170829-Rodin/smt1828334434025716862.smt2',
            'AUFLIA/20170829-Rodin/smt1829029808963281215.smt2',
            'AUFLIA/20170829-Rodin/smt1882594910272367920.smt2',
            'AUFLIA/20170829-Rodin/smt1883437689668062427.smt2'
            'AUFLIA/20170829-Rodin/smt2022486099146293362.smt2',
            'AUFLIA/20170829-Rodin/smt2025463987927880021.smt2',
            'AUFLIA/20170829-Rodin/smt2027035001350841448.smt2',
        ]

        for path in test_paths:
            test_path(path)


    def testParseLIA(self):
        test_paths = [
            'LIA/UltimateAutomizer/MADWiFi-encode_ie_ok_true-unreach-call.i_17.smt2',
            'LIA/UltimateAutomizer/Primes_true-unreach-call.c_678.smt2',
            'LIA/UltimateAutomizer/recHanoi03_true-unreach-call_true-termination.c_2175.smt2',
            'LIA/UltimateAutomizer/recHanoi03_true-unreach-call_true-termination.c_557.smt2',
        ]

        for path in test_paths:
            test_path(path)


    def testParseLRA(self):
        test_paths = [
            'LRA/scholl-smt08/RND/RND_3_15.smt2',
            'LRA/scholl-smt08/RND/RND_3_19.smt2',
            'LRA/scholl-smt08/RND/RND_3_28.smt2',
            'LRA/scholl-smt08/RND/RND_3_9.smt2',
            'LRA/scholl-smt08/RND/RND_4_12.smt2',
            'LRA/scholl-smt08/RND/RND_4_2.smt2',
            'LRA/scholl-smt08/RND/RND_4_7.smt2',
        ]

        for path in test_paths:
            test_path(path)

    def testParseBV(self):
        test_paths = [
            # 'BV/unsat/inf6.smt2', #pass
            # 'BV/unsat/inf8.smt2', #pass
            # 'BV/unsat/Prim_4.smt2', #pass
            # 'BV/unsat/intSqRoot.smt2', #pass
            # 'BV/unsat/bvnor.smt2', #pass
            # 'BV/unsat/bvsmod.smt2', #pass
            # 'BV/unsat/repeat.smt2', #pass
            # 'BV/unsat/bigHex.smt2', #SAT
            # 'BV/unsat/bvsdiv.smt2', #SAT
            # 'BV/unsat/symbols.smt2', #SAT
            # 'BV/unsat/extensions.smt2', #kill, but shouldnt
            # 'BV/unsat/modulus_true-unreach-call_true-no-overflow.i_242.smt2', #kill, but shouldnt

            # 'BV/20190429-UltimateAutomizerSvcomp2019/byte_add_1_true-unreach-call_true-no-overflow_true-termination.i_0.smt2',#kill

            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_1.smt2', #pass
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_2.smt2', #Undeclared variable bvlshr--too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_3.smt2', #pass
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_4.smt2', #pass
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_5.smt2', #pass
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_6.smt2', #pass
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_7.smt2', #Undeclared variable bvlshr -- developed , passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_8.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_9.smt2', #Undeclared variable bvshl -- developed , passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_10.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_11.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_12.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_13.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_14.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_15.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_16.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_17.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_18.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_19.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_20.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_21.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_22.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_23.smt2', #passed 
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_24.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_25.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_26.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_27.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_28.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_29.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_30.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_31.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_32.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_33.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_34.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_35.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_36.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_37.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_38.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_39.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_40.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_41.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_42.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_43.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_44.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_45.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_46.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_47.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_48.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_49.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_50.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_51.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_52.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_53.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_54.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_55.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_56.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_57.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_58.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_59.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_60.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_61.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_62.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_63.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_64.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_65.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_66.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_67.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_68.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_69.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_70.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_71.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_72.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_73.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_74.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_75.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_76.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_77.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_78.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_79.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_80.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_81.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_82.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_83.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_84.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_85.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_86.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_87.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_88.smt2', #AssertionError: bvlshr: argument2 is not word type
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_89.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_90.smt2', #AssertionError: bvshl: argument2 is not word type
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_91.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_92.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_93.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_94.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_95.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_96.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_97.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_98.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_99.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_100.smt2', #AssertionError: bvlshr: argument2 is not word type
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_101.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_102.smt2', #AssertionError: bvshl: argument2 is not word type
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_103.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_104.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_105.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_106.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_107.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_108.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_109.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_110.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_111.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_112.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_113.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_114.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_115.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_116.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_117.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_118.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_119.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_120.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_121.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_122.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_123.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_124.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_125.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_126.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_127.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_128.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_129.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_130.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_131.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_132.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_133.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_134.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_135.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_136.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_137.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_138.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_139.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_140.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_141.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_142.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_143.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_144.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_145.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_146.smt2', #passed
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_147.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_148.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_149.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_150.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_151.smt2', #passed SAT?
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_152.smt2', #too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_153.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_154.smt2', #AssertionError: bvand: argument is not word type -- and too slow
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_155.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_156.smt2', #kill
            # 'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_157.smt2', #kill #不应该kill，应该是未定义函数导致没有重构出来
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_158.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_159.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_160.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_161.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_162.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_163.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_164.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_165.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_166.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_167.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_168.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_169.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_170.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_171.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_172.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_173.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_174.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_175.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_176.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_177.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_178.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_179.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_180.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_181.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_182.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_183.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_184.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_185.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_186.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_187.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_188.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_189.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_190.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_191.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_192.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_193.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_194.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_195.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_196.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_197.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_198.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_199.smt2', #passed
            'BV/20190311-bv-term-small-rw-Noetzli/bv-term-small-rw_200.smt2', #passed

            # 'BV/unsat/counterexample.dump.ia32_Mul_base_disp--Add32.load32.Mul32.Mulh_u32.0005.smt2', #too large
            # 'BV/unsat/bitcount16.smt2', #kill
            # 'BV/unsat/bitcount32.smt2', #kill
            # 'BV/unsat/ex2_prime.smt2', #kill
            # 'BV/unsat/ex7_prime.smt2', #kill
            # 'BV/unsat/ex23.smt2', #kill
            # 'BV/unsat/ex30.smt2', #kill
            # 'BV/unsat/ex34.smt2', #kill
            # 'BV/unsat/ex37.smt2', #kill
            # 'BV/unsat/ex49.smt2', #kill
            # 'BV/unsat/simpleWhile.smt2', #kill
            # 'BV/unsat/predicate_125.smt2', #too large
        ]

        for path in test_paths:
            test_path(path, write_file=True)

    def testParseSMTAssertions(self):
        test_paths = [
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_16_59_39_090_1045351.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_17_00_12_337_1079814.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/abstract_completeness/x2015_09_10_17_00_49_980_1120402.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_52_18_634_983654.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_53_05_211_1033050.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/bindag/x2015_09_10_16_53_31_362_1064389.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_48_45_757_1043506.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_54_30_307_1349771.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/afp/coinductive_list/x2015_09_10_16_57_04_292_1481164.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_46_30_200_1001391.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_47_39_480_1078027.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/distro/gram_lang/x2015_09_10_16_48_44_767_1147663.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_54_35_132_1014381.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_54_53_474_1036287.smt_in.smt2',
            'UF/20170428-Barrett/cdt-cade2015/nada/gandl/bird_tree/x2015_09_10_16_55_00_922_1043783.smt_in.smt2',
            'UF/20201221-induction-by-reflection-schoisswohl/reflectiveConjecture/currying-0.smt2',
            'UF/grasshopper/instantiated/concat_check_heap_access_23_4.smt2',
            'UFLIA/boogie/AdditiveMethods_AdditiveMethods..ctor.smt2',
            'LIA/UltimateAutomizer/MADWiFi-encode_ie_ok_true-unreach-call.i_17.smt2',
        ]
        for path in test_paths:
            test_path(path, parse_assertion=True)

if __name__ == "__main__":
    unittest.main()
