(set-info :smt-lib-version 2.6)
(set-logic QF_LRA)
(set-info :source |
Fully parameterized specification and verification of a synchronizer   
circuit modeling metastability at various levels of refinement.     
A paper describing this specification, to be published in Designing     
Correct Circuits (DCC), 2006, is available from the authors.            

Geoffrey Brown, Indiana University <geobrown@cs.indiana.edu>
Lee Pike, Galois Connections, Inc. <leepike@galois.com>

Translated into CVC format by Leonardo de Moura.

This benchmark was automatically translated into SMT-LIB format from
CVC format using CVC Lite

|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun x_0 () Bool)
(declare-fun x_1 () Bool)
(declare-fun x_2 () Real)
(declare-fun x_3 () Bool)
(declare-fun x_4 () Bool)
(declare-fun x_5 () Bool)
(declare-fun x_6 () Real)
(declare-fun x_7 () Bool)
(declare-fun x_8 () Real)
(declare-fun x_9 () Real)
(declare-fun x_10 () Real)
(declare-fun x_11 () Real)
(declare-fun x_12 () Real)
(declare-fun x_13 () Bool)
(declare-fun x_14 () Real)
(declare-fun x_15 () Real)
(declare-fun x_16 () Real)
(declare-fun x_17 () Real)
(declare-fun x_18 () Real)
(declare-fun x_19 () Real)
(declare-fun x_20 () Bool)
(declare-fun x_21 () Bool)
(declare-fun x_22 () Bool)
(declare-fun x_23 () Bool)
(declare-fun x_24 () Real)
(declare-fun x_25 () Bool)
(declare-fun x_26 () Real)
(declare-fun x_27 () Real)
(declare-fun x_28 () Real)
(declare-fun x_29 () Real)
(declare-fun x_30 () Real)
(declare-fun x_31 () Real)
(declare-fun x_32 () Real)
(declare-fun x_33 () Real)
(declare-fun x_34 () Real)
(declare-fun x_35 () Real)
(declare-fun x_36 () Real)
(declare-fun x_37 () Bool)
(declare-fun x_38 () Real)
(declare-fun x_39 () Real)
(declare-fun x_40 () Bool)
(declare-fun x_41 () Bool)
(declare-fun x_42 () Bool)
(declare-fun x_43 () Bool)
(declare-fun x_44 () Real)
(declare-fun x_45 () Bool)
(declare-fun x_46 () Real)
(declare-fun x_47 () Real)
(declare-fun x_48 () Real)
(declare-fun x_49 () Real)
(declare-fun x_50 () Real)
(declare-fun x_51 () Real)
(declare-fun x_52 () Real)
(declare-fun x_53 () Real)
(declare-fun x_54 () Real)
(declare-fun x_55 () Bool)
(declare-fun x_56 () Real)
(declare-fun x_57 () Real)
(declare-fun x_58 () Bool)
(declare-fun x_59 () Bool)
(declare-fun x_60 () Bool)
(declare-fun x_61 () Bool)
(declare-fun x_62 () Real)
(declare-fun x_63 () Bool)
(declare-fun x_64 () Real)
(declare-fun x_65 () Real)
(declare-fun x_66 () Real)
(declare-fun x_67 () Real)
(declare-fun x_68 () Real)
(declare-fun x_69 () Real)
(declare-fun x_70 () Real)
(declare-fun x_71 () Real)
(declare-fun x_72 () Real)
(declare-fun x_73 () Bool)
(declare-fun x_74 () Real)
(declare-fun x_75 () Real)
(declare-fun x_76 () Bool)
(declare-fun x_77 () Bool)
(declare-fun x_78 () Bool)
(declare-fun x_79 () Bool)
(declare-fun x_80 () Real)
(declare-fun x_81 () Bool)
(declare-fun x_82 () Real)
(declare-fun x_83 () Real)
(declare-fun x_84 () Real)
(declare-fun x_85 () Real)
(declare-fun x_86 () Real)
(declare-fun x_87 () Real)
(declare-fun x_88 () Real)
(declare-fun x_89 () Real)
(declare-fun x_90 () Real)
(declare-fun x_91 () Bool)
(declare-fun x_92 () Real)
(declare-fun x_93 () Real)
(declare-fun x_94 () Bool)
(declare-fun x_95 () Bool)
(declare-fun x_96 () Bool)
(declare-fun x_97 () Bool)
(declare-fun x_98 () Real)
(declare-fun x_99 () Bool)
(declare-fun x_100 () Real)
(declare-fun x_101 () Real)
(declare-fun x_102 () Real)
(declare-fun x_103 () Real)
(declare-fun x_104 () Real)
(declare-fun x_105 () Real)
(declare-fun x_106 () Real)
(declare-fun x_107 () Real)
(declare-fun x_108 () Real)
(declare-fun x_109 () Bool)
(declare-fun x_110 () Real)
(declare-fun x_111 () Real)
(declare-fun x_112 () Bool)
(declare-fun x_113 () Bool)
(declare-fun x_114 () Bool)
(declare-fun x_115 () Bool)
(declare-fun x_116 () Real)
(declare-fun x_117 () Bool)
(declare-fun x_118 () Real)
(declare-fun x_119 () Real)
(declare-fun x_120 () Real)
(declare-fun x_121 () Real)
(declare-fun x_122 () Real)
(declare-fun x_123 () Real)
(declare-fun x_124 () Real)
(declare-fun x_125 () Real)
(declare-fun x_126 () Real)
(declare-fun x_127 () Bool)
(declare-fun x_128 () Real)
(declare-fun x_129 () Real)
(declare-fun x_130 () Bool)
(declare-fun x_131 () Bool)
(declare-fun x_132 () Bool)
(declare-fun x_133 () Bool)
(declare-fun x_134 () Real)
(declare-fun x_135 () Bool)
(declare-fun x_136 () Real)
(declare-fun x_137 () Real)
(declare-fun x_138 () Real)
(declare-fun x_139 () Real)
(declare-fun x_140 () Real)
(declare-fun x_141 () Real)
(declare-fun x_142 () Real)
(declare-fun x_143 () Real)
(declare-fun x_144 () Real)
(declare-fun x_145 () Bool)
(declare-fun x_146 () Real)
(declare-fun x_147 () Real)
(declare-fun x_148 () Bool)
(declare-fun x_149 () Bool)
(declare-fun x_150 () Bool)
(declare-fun x_151 () Bool)
(declare-fun x_152 () Real)
(declare-fun x_153 () Bool)
(declare-fun x_154 () Real)
(declare-fun x_155 () Real)
(declare-fun x_156 () Real)
(declare-fun x_157 () Real)
(declare-fun x_158 () Real)
(declare-fun x_159 () Real)
(declare-fun x_160 () Real)
(declare-fun x_161 () Real)
(declare-fun x_162 () Real)
(declare-fun x_163 () Bool)
(declare-fun x_164 () Real)
(declare-fun x_165 () Real)
(declare-fun x_166 () Bool)
(declare-fun x_167 () Bool)
(declare-fun x_168 () Bool)
(declare-fun x_169 () Bool)
(declare-fun x_170 () Real)
(declare-fun x_171 () Bool)
(declare-fun x_172 () Real)
(declare-fun x_173 () Real)
(declare-fun x_174 () Real)
(declare-fun x_175 () Real)
(declare-fun x_176 () Real)
(declare-fun x_177 () Real)
(declare-fun x_178 () Real)
(declare-fun x_179 () Real)
(declare-fun x_180 () Real)
(declare-fun x_181 () Bool)
(declare-fun x_182 () Real)
(declare-fun x_183 () Real)
(declare-fun x_184 () Bool)
(declare-fun x_185 () Bool)
(declare-fun x_186 () Bool)
(declare-fun x_187 () Bool)
(declare-fun x_188 () Real)
(declare-fun x_189 () Bool)
(declare-fun x_190 () Real)
(declare-fun x_191 () Real)
(declare-fun x_192 () Real)
(declare-fun x_193 () Real)
(assert (let ((?v_125 (= x_6 x_9)) (?v_122 (= x_2 x_11))) (let ((?v_127 (not ?v_122)) (?v_118 (= x_13 x_7)) (?v_119 (= x_14 x_15)) (?v_123 (ite (<= x_6 x_2) x_6 x_2)) (?v_117 (ite (<= x_2 x_6) x_2 x_6)) (?v_110 (= x_32 x_8)) (?v_111 (= x_9 x_33)) (?v_112 (= x_34 x_10)) (?v_108 (= x_11 x_35))) (let ((?v_113 (not ?v_108)) (?v_116 (= x_36 x_12)) (?v_105 (= x_37 x_13)) (?v_106 (= x_38 x_14)) (?v_109 (ite (<= x_9 x_11) x_9 x_11)) (?v_104 (ite (<= x_11 x_9) x_11 x_9)) (?v_97 (= x_50 x_32)) (?v_98 (= x_33 x_51)) (?v_99 (= x_52 x_34)) (?v_95 (= x_35 x_53))) (let ((?v_100 (not ?v_95)) (?v_103 (= x_54 x_36)) (?v_92 (= x_55 x_37)) (?v_93 (= x_56 x_38)) (?v_96 (ite (<= x_33 x_35) x_33 x_35)) (?v_91 (ite (<= x_35 x_33) x_35 x_33)) (?v_84 (= x_68 x_50)) (?v_85 (= x_51 x_69)) (?v_86 (= x_70 x_52)) (?v_82 (= x_53 x_71))) (let ((?v_87 (not ?v_82)) (?v_90 (= x_72 x_54)) (?v_79 (= x_73 x_55)) (?v_80 (= x_74 x_56)) (?v_83 (ite (<= x_51 x_53) x_51 x_53)) (?v_78 (ite (<= x_53 x_51) x_53 x_51)) (?v_71 (= x_86 x_68)) (?v_72 (= x_69 x_87)) (?v_73 (= x_88 x_70)) (?v_69 (= x_71 x_89))) (let ((?v_74 (not ?v_69)) (?v_77 (= x_90 x_72)) (?v_66 (= x_91 x_73)) (?v_67 (= x_92 x_74)) (?v_70 (ite (<= x_69 x_71) x_69 x_71)) (?v_65 (ite (<= x_71 x_69) x_71 x_69)) (?v_58 (= x_104 x_86)) (?v_59 (= x_87 x_105)) (?v_60 (= x_106 x_88)) (?v_56 (= x_89 x_107))) (let ((?v_61 (not ?v_56)) (?v_64 (= x_108 x_90)) (?v_53 (= x_109 x_91)) (?v_54 (= x_110 x_92)) (?v_57 (ite (<= x_87 x_89) x_87 x_89)) (?v_52 (ite (<= x_89 x_87) x_89 x_87)) (?v_45 (= x_122 x_104)) (?v_46 (= x_105 x_123)) (?v_47 (= x_124 x_106)) (?v_43 (= x_107 x_125))) (let ((?v_48 (not ?v_43)) (?v_51 (= x_126 x_108)) (?v_40 (= x_127 x_109)) (?v_41 (= x_128 x_110)) (?v_44 (ite (<= x_105 x_107) x_105 x_107)) (?v_39 (ite (<= x_107 x_105) x_107 x_105)) (?v_32 (= x_140 x_122)) (?v_33 (= x_123 x_141)) (?v_34 (= x_142 x_124)) (?v_30 (= x_125 x_143))) (let ((?v_35 (not ?v_30)) (?v_38 (= x_144 x_126)) (?v_27 (= x_145 x_127)) (?v_28 (= x_146 x_128)) (?v_31 (ite (<= x_123 x_125) x_123 x_125)) (?v_26 (ite (<= x_125 x_123) x_125 x_123)) (?v_19 (= x_158 x_140)) (?v_20 (= x_141 x_159)) (?v_21 (= x_160 x_142)) (?v_17 (= x_143 x_161))) (let ((?v_22 (not ?v_17)) (?v_25 (= x_162 x_144)) (?v_14 (= x_163 x_145)) (?v_15 (= x_164 x_146)) (?v_18 (ite (<= x_141 x_143) x_141 x_143)) (?v_13 (ite (<= x_143 x_141) x_143 x_141)) (?v_6 (= x_176 x_158)) (?v_7 (= x_159 x_177)) (?v_8 (= x_178 x_160)) (?v_4 (= x_161 x_179))) (let ((?v_9 (not ?v_4)) (?v_12 (= x_180 x_162)) (?v_1 (= x_181 x_163)) (?v_2 (= x_182 x_164)) (?v_5 (ite (<= x_159 x_161) x_159 x_161)) (?v_0 (ite (<= x_161 x_159) x_161 x_159)) (?v_120 (not x_7)) (?v_3 (= x_185 x_167)) (?v_11 (= x_164 x_182)) (?v_16 (= x_167 x_149)) (?v_24 (= x_146 x_164)) (?v_29 (= x_149 x_131)) (?v_37 (= x_128 x_146)) (?v_42 (= x_131 x_113)) (?v_50 (= x_110 x_128)) (?v_55 (= x_113 x_95)) (?v_63 (= x_92 x_110)) (?v_68 (= x_95 x_77)) (?v_76 (= x_74 x_92)) (?v_81 (= x_77 x_59)) (?v_89 (= x_56 x_74)) (?v_94 (= x_59 x_41)) (?v_102 (= x_38 x_56)) (?v_107 (= x_41 x_21)) (?v_115 (= x_14 x_38)) (?v_121 (= x_21 x_3)) (?v_130 (= x_15 x_14)) (?v_128 (not (>= ?v_123 0))) (?v_124 (= x_8 0)) (?v_126 (= x_10 0)) (?v_131 (= x_12 0))) (let ((?v_10 (+ ?v_5 x_18)) (?v_23 (+ ?v_18 x_18)) (?v_36 (+ ?v_31 x_18)) (?v_49 (+ ?v_44 x_18)) (?v_62 (+ ?v_57 x_18)) (?v_75 (+ ?v_70 x_18)) (?v_88 (+ ?v_83 x_18)) (?v_101 (+ ?v_96 x_18)) (?v_114 (+ ?v_109 x_18)) (?v_129 (+ ?v_123 x_18))) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> x_16 0) (> x_17 0)) (>= x_18 0)) (< x_18 x_16)) (< x_18 x_17)) (>= x_30 0)) (< x_30 x_16)) (< x_30 x_17)) (not x_0)) (not x_1)) (>= x_2 0)) (not x_3)) (not x_4)) (not x_5)) (>= x_6 0)) ?v_120) (or (and (and (and (and (and (and (and (and (and (= x_183 0) (= x_184 x_171)) (= ?v_0 x_161)) (<= (+ ?v_0 x_16) x_179)) (or ?v_3 (= x_185 x_166))) (= x_186 x_168)) (= x_187 x_169)) (= x_177 x_159)) ?v_1) ?v_2) (and (and (and (and (and (and (and (and (and (= x_183 1) (= x_187 x_168)) (= ?v_0 x_159)) (<= (+ ?v_0 x_17) x_177)) (or (and (and (= x_188 0) ?v_1) ?v_2) (and (and (= x_188 1) (= x_163 x_169)) (= x_181 (not x_163))))) (= x_189 x_171)) (= x_184 x_166)) (= x_190 x_172)) (= x_179 x_161)) ?v_3))) (or (or (and (and (and (= x_191 0) ?v_9) (or (not (<= x_158 ?v_5)) (= x_189 x_163))) ?v_6) (and (and (and (= x_191 1) ?v_4) (xor x_163 x_181)) (= x_176 ?v_10))) (and (and (and (= x_191 2) ?v_4) (= x_163 x_181)) ?v_6))) (or (or (and (and (and (= x_192 0) (not ?v_7)) (or (not (<= x_160 ?v_0)) (= x_186 x_167))) ?v_8) (and (and (and (= x_192 1) ?v_7) (xor x_167 x_185)) (= x_178 (+ ?v_0 x_30)))) (and (and (and (= x_192 2) ?v_7) (= x_167 x_185)) ?v_8))) (or (or (and (and (and (= x_193 0) ?v_9) (or (not (<= x_162 ?v_5)) (= x_190 x_164))) ?v_12) (and (and (and (= x_193 1) ?v_4) (not ?v_11)) (= x_180 ?v_10))) (and (and (and (= x_193 2) ?v_4) ?v_11) ?v_12))) (or (and (and (and (and (and (and (and (and (and (= x_165 0) (= x_166 x_153)) (= ?v_13 x_143)) (<= (+ ?v_13 x_16) x_161)) (or ?v_16 (= x_167 x_148))) (= x_168 x_150)) (= x_169 x_151)) (= x_159 x_141)) ?v_14) ?v_15) (and (and (and (and (and (and (and (and (and (= x_165 1) (= x_169 x_150)) (= ?v_13 x_141)) (<= (+ ?v_13 x_17) x_159)) (or (and (and (= x_170 0) ?v_14) ?v_15) (and (and (= x_170 1) (= x_145 x_151)) (= x_163 (not x_145))))) (= x_171 x_153)) (= x_166 x_148)) (= x_172 x_154)) (= x_161 x_143)) ?v_16))) (or (or (and (and (and (= x_173 0) ?v_22) (or (not (<= x_140 ?v_18)) (= x_171 x_145))) ?v_19) (and (and (and (= x_173 1) ?v_17) (xor x_145 x_163)) (= x_158 ?v_23))) (and (and (and (= x_173 2) ?v_17) (= x_145 x_163)) ?v_19))) (or (or (and (and (and (= x_174 0) (not ?v_20)) (or (not (<= x_142 ?v_13)) (= x_168 x_149))) ?v_21) (and (and (and (= x_174 1) ?v_20) (xor x_149 x_167)) (= x_160 (+ ?v_13 x_30)))) (and (and (and (= x_174 2) ?v_20) (= x_149 x_167)) ?v_21))) (or (or (and (and (and (= x_175 0) ?v_22) (or (not (<= x_144 ?v_18)) (= x_172 x_146))) ?v_25) (and (and (and (= x_175 1) ?v_17) (not ?v_24)) (= x_162 ?v_23))) (and (and (and (= x_175 2) ?v_17) ?v_24) ?v_25))) (or (and (and (and (and (and (and (and (and (and (= x_147 0) (= x_148 x_135)) (= ?v_26 x_125)) (<= (+ ?v_26 x_16) x_143)) (or ?v_29 (= x_149 x_130))) (= x_150 x_132)) (= x_151 x_133)) (= x_141 x_123)) ?v_27) ?v_28) (and (and (and (and (and (and (and (and (and (= x_147 1) (= x_151 x_132)) (= ?v_26 x_123)) (<= (+ ?v_26 x_17) x_141)) (or (and (and (= x_152 0) ?v_27) ?v_28) (and (and (= x_152 1) (= x_127 x_133)) (= x_145 (not x_127))))) (= x_153 x_135)) (= x_148 x_130)) (= x_154 x_136)) (= x_143 x_125)) ?v_29))) (or (or (and (and (and (= x_155 0) ?v_35) (or (not (<= x_122 ?v_31)) (= x_153 x_127))) ?v_32) (and (and (and (= x_155 1) ?v_30) (xor x_127 x_145)) (= x_140 ?v_36))) (and (and (and (= x_155 2) ?v_30) (= x_127 x_145)) ?v_32))) (or (or (and (and (and (= x_156 0) (not ?v_33)) (or (not (<= x_124 ?v_26)) (= x_150 x_131))) ?v_34) (and (and (and (= x_156 1) ?v_33) (xor x_131 x_149)) (= x_142 (+ ?v_26 x_30)))) (and (and (and (= x_156 2) ?v_33) (= x_131 x_149)) ?v_34))) (or (or (and (and (and (= x_157 0) ?v_35) (or (not (<= x_126 ?v_31)) (= x_154 x_128))) ?v_38) (and (and (and (= x_157 1) ?v_30) (not ?v_37)) (= x_144 ?v_36))) (and (and (and (= x_157 2) ?v_30) ?v_37) ?v_38))) (or (and (and (and (and (and (and (and (and (and (= x_129 0) (= x_130 x_117)) (= ?v_39 x_107)) (<= (+ ?v_39 x_16) x_125)) (or ?v_42 (= x_131 x_112))) (= x_132 x_114)) (= x_133 x_115)) (= x_123 x_105)) ?v_40) ?v_41) (and (and (and (and (and (and (and (and (and (= x_129 1) (= x_133 x_114)) (= ?v_39 x_105)) (<= (+ ?v_39 x_17) x_123)) (or (and (and (= x_134 0) ?v_40) ?v_41) (and (and (= x_134 1) (= x_109 x_115)) (= x_127 (not x_109))))) (= x_135 x_117)) (= x_130 x_112)) (= x_136 x_118)) (= x_125 x_107)) ?v_42))) (or (or (and (and (and (= x_137 0) ?v_48) (or (not (<= x_104 ?v_44)) (= x_135 x_109))) ?v_45) (and (and (and (= x_137 1) ?v_43) (xor x_109 x_127)) (= x_122 ?v_49))) (and (and (and (= x_137 2) ?v_43) (= x_109 x_127)) ?v_45))) (or (or (and (and (and (= x_138 0) (not ?v_46)) (or (not (<= x_106 ?v_39)) (= x_132 x_113))) ?v_47) (and (and (and (= x_138 1) ?v_46) (xor x_113 x_131)) (= x_124 (+ ?v_39 x_30)))) (and (and (and (= x_138 2) ?v_46) (= x_113 x_131)) ?v_47))) (or (or (and (and (and (= x_139 0) ?v_48) (or (not (<= x_108 ?v_44)) (= x_136 x_110))) ?v_51) (and (and (and (= x_139 1) ?v_43) (not ?v_50)) (= x_126 ?v_49))) (and (and (and (= x_139 2) ?v_43) ?v_50) ?v_51))) (or (and (and (and (and (and (and (and (and (and (= x_111 0) (= x_112 x_99)) (= ?v_52 x_89)) (<= (+ ?v_52 x_16) x_107)) (or ?v_55 (= x_113 x_94))) (= x_114 x_96)) (= x_115 x_97)) (= x_105 x_87)) ?v_53) ?v_54) (and (and (and (and (and (and (and (and (and (= x_111 1) (= x_115 x_96)) (= ?v_52 x_87)) (<= (+ ?v_52 x_17) x_105)) (or (and (and (= x_116 0) ?v_53) ?v_54) (and (and (= x_116 1) (= x_91 x_97)) (= x_109 (not x_91))))) (= x_117 x_99)) (= x_112 x_94)) (= x_118 x_100)) (= x_107 x_89)) ?v_55))) (or (or (and (and (and (= x_119 0) ?v_61) (or (not (<= x_86 ?v_57)) (= x_117 x_91))) ?v_58) (and (and (and (= x_119 1) ?v_56) (xor x_91 x_109)) (= x_104 ?v_62))) (and (and (and (= x_119 2) ?v_56) (= x_91 x_109)) ?v_58))) (or (or (and (and (and (= x_120 0) (not ?v_59)) (or (not (<= x_88 ?v_52)) (= x_114 x_95))) ?v_60) (and (and (and (= x_120 1) ?v_59) (xor x_95 x_113)) (= x_106 (+ ?v_52 x_30)))) (and (and (and (= x_120 2) ?v_59) (= x_95 x_113)) ?v_60))) (or (or (and (and (and (= x_121 0) ?v_61) (or (not (<= x_90 ?v_57)) (= x_118 x_92))) ?v_64) (and (and (and (= x_121 1) ?v_56) (not ?v_63)) (= x_108 ?v_62))) (and (and (and (= x_121 2) ?v_56) ?v_63) ?v_64))) (or (and (and (and (and (and (and (and (and (and (= x_93 0) (= x_94 x_81)) (= ?v_65 x_71)) (<= (+ ?v_65 x_16) x_89)) (or ?v_68 (= x_95 x_76))) (= x_96 x_78)) (= x_97 x_79)) (= x_87 x_69)) ?v_66) ?v_67) (and (and (and (and (and (and (and (and (and (= x_93 1) (= x_97 x_78)) (= ?v_65 x_69)) (<= (+ ?v_65 x_17) x_87)) (or (and (and (= x_98 0) ?v_66) ?v_67) (and (and (= x_98 1) (= x_73 x_79)) (= x_91 (not x_73))))) (= x_99 x_81)) (= x_94 x_76)) (= x_100 x_82)) (= x_89 x_71)) ?v_68))) (or (or (and (and (and (= x_101 0) ?v_74) (or (not (<= x_68 ?v_70)) (= x_99 x_73))) ?v_71) (and (and (and (= x_101 1) ?v_69) (xor x_73 x_91)) (= x_86 ?v_75))) (and (and (and (= x_101 2) ?v_69) (= x_73 x_91)) ?v_71))) (or (or (and (and (and (= x_102 0) (not ?v_72)) (or (not (<= x_70 ?v_65)) (= x_96 x_77))) ?v_73) (and (and (and (= x_102 1) ?v_72) (xor x_77 x_95)) (= x_88 (+ ?v_65 x_30)))) (and (and (and (= x_102 2) ?v_72) (= x_77 x_95)) ?v_73))) (or (or (and (and (and (= x_103 0) ?v_74) (or (not (<= x_72 ?v_70)) (= x_100 x_74))) ?v_77) (and (and (and (= x_103 1) ?v_69) (not ?v_76)) (= x_90 ?v_75))) (and (and (and (= x_103 2) ?v_69) ?v_76) ?v_77))) (or (and (and (and (and (and (and (and (and (and (= x_75 0) (= x_76 x_63)) (= ?v_78 x_53)) (<= (+ ?v_78 x_16) x_71)) (or ?v_81 (= x_77 x_58))) (= x_78 x_60)) (= x_79 x_61)) (= x_69 x_51)) ?v_79) ?v_80) (and (and (and (and (and (and (and (and (and (= x_75 1) (= x_79 x_60)) (= ?v_78 x_51)) (<= (+ ?v_78 x_17) x_69)) (or (and (and (= x_80 0) ?v_79) ?v_80) (and (and (= x_80 1) (= x_55 x_61)) (= x_73 (not x_55))))) (= x_81 x_63)) (= x_76 x_58)) (= x_82 x_64)) (= x_71 x_53)) ?v_81))) (or (or (and (and (and (= x_83 0) ?v_87) (or (not (<= x_50 ?v_83)) (= x_81 x_55))) ?v_84) (and (and (and (= x_83 1) ?v_82) (xor x_55 x_73)) (= x_68 ?v_88))) (and (and (and (= x_83 2) ?v_82) (= x_55 x_73)) ?v_84))) (or (or (and (and (and (= x_84 0) (not ?v_85)) (or (not (<= x_52 ?v_78)) (= x_78 x_59))) ?v_86) (and (and (and (= x_84 1) ?v_85) (xor x_59 x_77)) (= x_70 (+ ?v_78 x_30)))) (and (and (and (= x_84 2) ?v_85) (= x_59 x_77)) ?v_86))) (or (or (and (and (and (= x_85 0) ?v_87) (or (not (<= x_54 ?v_83)) (= x_82 x_56))) ?v_90) (and (and (and (= x_85 1) ?v_82) (not ?v_89)) (= x_72 ?v_88))) (and (and (and (= x_85 2) ?v_82) ?v_89) ?v_90))) (or (and (and (and (and (and (and (and (and (and (= x_57 0) (= x_58 x_45)) (= ?v_91 x_35)) (<= (+ ?v_91 x_16) x_53)) (or ?v_94 (= x_59 x_40))) (= x_60 x_42)) (= x_61 x_43)) (= x_51 x_33)) ?v_92) ?v_93) (and (and (and (and (and (and (and (and (and (= x_57 1) (= x_61 x_42)) (= ?v_91 x_33)) (<= (+ ?v_91 x_17) x_51)) (or (and (and (= x_62 0) ?v_92) ?v_93) (and (and (= x_62 1) (= x_37 x_43)) (= x_55 (not x_37))))) (= x_63 x_45)) (= x_58 x_40)) (= x_64 x_46)) (= x_53 x_35)) ?v_94))) (or (or (and (and (and (= x_65 0) ?v_100) (or (not (<= x_32 ?v_96)) (= x_63 x_37))) ?v_97) (and (and (and (= x_65 1) ?v_95) (xor x_37 x_55)) (= x_50 ?v_101))) (and (and (and (= x_65 2) ?v_95) (= x_37 x_55)) ?v_97))) (or (or (and (and (and (= x_66 0) (not ?v_98)) (or (not (<= x_34 ?v_91)) (= x_60 x_41))) ?v_99) (and (and (and (= x_66 1) ?v_98) (xor x_41 x_59)) (= x_52 (+ ?v_91 x_30)))) (and (and (and (= x_66 2) ?v_98) (= x_41 x_59)) ?v_99))) (or (or (and (and (and (= x_67 0) ?v_100) (or (not (<= x_36 ?v_96)) (= x_64 x_38))) ?v_103) (and (and (and (= x_67 1) ?v_95) (not ?v_102)) (= x_54 ?v_101))) (and (and (and (= x_67 2) ?v_95) ?v_102) ?v_103))) (or (and (and (and (and (and (and (and (and (and (= x_39 0) (= x_40 x_25)) (= ?v_104 x_11)) (<= (+ ?v_104 x_16) x_35)) (or ?v_107 (= x_41 x_20))) (= x_42 x_22)) (= x_43 x_23)) (= x_33 x_9)) ?v_105) ?v_106) (and (and (and (and (and (and (and (and (and (= x_39 1) (= x_43 x_22)) (= ?v_104 x_9)) (<= (+ ?v_104 x_17) x_33)) (or (and (and (= x_44 0) ?v_105) ?v_106) (and (and (= x_44 1) (= x_13 x_23)) (= x_37 (not x_13))))) (= x_45 x_25)) (= x_40 x_20)) (= x_46 x_26)) (= x_35 x_11)) ?v_107))) (or (or (and (and (and (= x_47 0) ?v_113) (or (not (<= x_8 ?v_109)) (= x_45 x_13))) ?v_110) (and (and (and (= x_47 1) ?v_108) (xor x_13 x_37)) (= x_32 ?v_114))) (and (and (and (= x_47 2) ?v_108) (= x_13 x_37)) ?v_110))) (or (or (and (and (and (= x_48 0) (not ?v_111)) (or (not (<= x_10 ?v_104)) (= x_42 x_21))) ?v_112) (and (and (and (= x_48 1) ?v_111) (xor x_21 x_41)) (= x_34 (+ ?v_104 x_30)))) (and (and (and (= x_48 2) ?v_111) (= x_21 x_41)) ?v_112))) (or (or (and (and (and (= x_49 0) ?v_113) (or (not (<= x_12 ?v_109)) (= x_46 x_14))) ?v_116) (and (and (and (= x_49 1) ?v_108) (not ?v_115)) (= x_36 ?v_114))) (and (and (and (= x_49 2) ?v_108) ?v_115) ?v_116))) (or (and (and (and (and (and (and (and (and (and (= x_19 0) (= x_20 x_0)) (= ?v_117 x_2)) (<= (+ ?v_117 x_16) x_11)) (or ?v_121 (= x_21 x_1))) (= x_22 x_4)) (= x_23 x_5)) (= x_9 x_6)) ?v_118) ?v_119) (and (and (and (and (and (and (and (and (and (= x_19 1) (= x_23 x_4)) (= ?v_117 x_6)) (<= (+ ?v_117 x_17) x_9)) (or (and (and (= x_24 0) ?v_118) ?v_119) (and (and (= x_24 1) (= x_7 x_5)) (= x_13 ?v_120)))) (= x_25 x_0)) (= x_20 x_1)) (= x_26 x_27)) (= x_11 x_2)) ?v_121))) (or (or (and (and (and (= x_28 0) ?v_127) (or ?v_128 (= x_25 x_7))) ?v_124) (and (and (and (= x_28 1) ?v_122) (xor x_7 x_13)) (= x_8 ?v_129))) (and (and (and (= x_28 2) ?v_122) (= x_7 x_13)) ?v_124))) (or (or (and (and (and (= x_29 0) (not ?v_125)) (or (not (>= ?v_117 0)) (= x_22 x_3))) ?v_126) (and (and (and (= x_29 1) ?v_125) (xor x_3 x_21)) (= x_10 (+ ?v_117 x_30)))) (and (and (and (= x_29 2) ?v_125) (= x_3 x_21)) ?v_126))) (or (or (and (and (and (= x_31 0) ?v_127) (or ?v_128 (= x_26 x_15))) ?v_131) (and (and (and (= x_31 1) ?v_122) (not ?v_130)) (= x_12 ?v_129))) (and (and (and (= x_31 2) ?v_122) ?v_130) ?v_131))) (or (or (or (or (or (or (or (or (or (or (and (xor x_184 x_185) (not (= x_190 x_182))) (and (xor x_166 x_167) (not (= x_172 x_164)))) (and (xor x_148 x_149) (not (= x_154 x_146)))) (and (xor x_130 x_131) (not (= x_136 x_128)))) (and (xor x_112 x_113) (not (= x_118 x_110)))) (and (xor x_94 x_95) (not (= x_100 x_92)))) (and (xor x_76 x_77) (not (= x_82 x_74)))) (and (xor x_58 x_59) (not (= x_64 x_56)))) (and (xor x_40 x_41) (not (= x_46 x_38)))) (and (xor x_20 x_21) (not (= x_26 x_14)))) (and (xor x_1 x_3) (not (= x_27 x_15))))))))))))))))))
(check-sat)
(exit)