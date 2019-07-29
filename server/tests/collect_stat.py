# Author: Bohua Zhan

"""Statistics of common theorems."""

import unittest

from server.tests.server_test import testMethods

test_theorems = [
    # Logic
    ('logic', 'double_neg'),
    ('logic', 'disj_conv_imp'),
    ('logic', 'ex_conj_distrib'),
    ('logic', 'all_conj_distrib'),
    ('logic', 'conj_disj_distribL1'),
    ('logic', 'pierce'),
    ('logic', 'drinker'),
    # Sets
    ('set', 'subset_antisym'),
    ('set', 'subset_trans'),
    ('set', 'cantor'),
    ('set', 'Inter_subset'),
    ('set', 'subset_Inter'),
    ('set', 'Union_union'),
    ('set', 'lfp_lowerbound'),
    ('set', 'lfp_greatest'),
    ('set', 'lfp_unfold'),
    # Functions
    ('function', 'fun_upd_triv'),
    ('function', 'fun_upd_upd'),
    ('function', 'fun_upd_twist'),
    ('function', 'comp_fun_assoc'),
    ('function', 'injective_comp_fun'),
    ('function', 'surjective_comp_fun'),
    # Peano arithmetic
    ('nat', 'add_comm'),
    ('nat', 'add_assoc'),
    ('nat', 'distrib_l'),
    ('nat', 'mult_assoc'),
    ('nat', 'mult_comm'),
    ('nat', 'less_eq_trans'),
    # Lists
    ('list', 'append_right_neutral'),
    ('list', 'append_assoc'),
    ('list', 'length_append'),
    ('list', 'rev_append'),
    ('list', 'rev_rev'),
    ('list', 'rev_length'),
]

class CollectStat(unittest.TestCase):
    def testCollectStat(self):
        for thy_name, thm_name in test_theorems:
            testMethods(self, thy_name, thm_name, no_gaps=True, print_stat=True)