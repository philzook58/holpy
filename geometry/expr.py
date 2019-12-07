"""Expressions in geometry prover."""

import itertools, copy

POINT, LINE, PonL, SEG, TRI, CIRC, CYCL = range(7)


def get_arg_type_by_fact(fact):
    """Obtain the type of arguments for the given fact.

    This is determined by the pred_name of the fact, as well as
    upper/lower case of the arguments.

    Return the argument type.

    """
    pred_name = fact.pred_name

    if pred_name in ("para", "perp", "eqangle"):
        if fact.args[0].isupper():
            return PonL
        else:
            return LINE
    elif pred_name in ("coll", "midp"):
        return POINT
    elif pred_name in ("eqratio", "cong"):
        return SEG
    elif pred_name == "cyclic":
        return CYCL
    elif pred_name == "circle":
        return CIRC
    elif pred_name == "simtri":
        return TRI
    else:
        raise NotImplementedError


divisors = {
    "para": 2, "perp": 2, "eqangle": 4, "coll": 1, "midp": 1,
    "eqratio": 4, "cong": 2, "cyclic": 0, "circle": 0, "simtri": 3}

divisors_for_apply = {
    "para": 2, "perp": 2, "eqangle": 4, "coll": 1, "midp": 1,
    "eqratio": 2, "cong": 2, "cyclic": 1, "circle": 1, "simtri": 3}


class Fact:
    """Represent a fact in geometry prover, e.g.:

    coll(A, C, B) is Fact("coll", ["A", "C", "B"]).

    updated: Whether this fact is generated by prover or not.
    lemma: An integer that record which rule is it required to obtain this fact (its place in ruleset).
            None represents no requirement.
    cond: A list of integers that record what facts (their place in fact list) are required to obtain this fact.
            Use "default" when initializing.
            number -1 represents no requirement.
    """

    def __init__(self, pred_name, args, *, updated=False, lemma=None, cond=[], pos=[]):
        assert isinstance(pred_name, str)
        assert isinstance(args, list) and all(isinstance(arg, str) for arg in args)
        self.pred_name = pred_name
        self.args = args
        self.updated = updated
        self.lemma = lemma
        if pos == "default":
            self.pos = self.get_default()
        else:
            self.pos = pos
        if cond == "default":
            self.cond = self.get_default()
        else:
            self.cond = cond


    def __hash__(self):
        return hash(("Fact", self.pred_name, tuple(self.args)))

    def __eq__(self, other):
        return isinstance(other, Fact) and self.pred_name == other.pred_name and \
               self.args == other.args

    def __str__(self):
        return "%s(%s)" % (self.pred_name, ",".join(self.args))

    def __repr__(self):
        return str(self)

    def get_default(self):
        d = divisors[self.pred_name]
        if d == 0:
            return [-1] * len(self.args)
        else:
            assert len(self.args) % d == 0
            return [[-1] for _ in range(len(self.args) // d)]


class Line:
    """Represent a line contains more than one point."""
    def __init__(self, args, source=None):
        assert isinstance(args, list)
        assert len(args) > 1
        assert all(isinstance(arg, str) for arg in args)
        self.args = set(args)
        self.source = source

    def __hash__(self):
        return hash(("line", tuple(sorted(self.args))))

    def __eq__(self, other):
        return isinstance(other, Line) and self.args == other.args

    def __str__(self):
        return "Line(%s)" % (",".join(self.args))

    def __repr__(self):
        return str(self)

    def is_same_line(self, other):
        # Two lines are same if they have at least 2 identical points.
        return isinstance(other, Line) and len(self.args.intersection(other.args)) >= 2

    def combine(self, other):
        # If the other line refers to the same line of this line,
        # add the points of other line that are not in this line.
        assert self.is_same_line(other), "combine line"
        if self.args != other.args:
            self.args = self.args.union(other.args)


class Circle:
    """Represent a circle."""
    def __init__(self, args, center=None, source=None):
        assert isinstance(args, list)
        assert len(args) >= 3
        assert all(isinstance(arg, str) for arg in args)
        self.args = set(args)
        self.source = source
        self.center = center

    def __hash__(self):
        return hash(("circle", self.center, tuple(sorted(self.args))))

    def __eq__(self, other):
        if self.center and other.center and self.center != other.center:
            return False
        return isinstance(other, Circle) and self.args == other.args

    def __str__(self):
        return "Circle(%s,%s)" % (self.center, ",".join(self.args))

    def __repr__(self):
        return str(self)

    def is_same_circle(self, other):
        """Two circles are the same if they have 3 or more identical points.
        If both circles have center and they have 3 or more identical points
        then two centers must be the same.
        """
        if isinstance(other, Circle) and len(self.args.intersection(other.args)) >= 3:
            if self.center and other.center:
                return self.center == other.center
            else:
                return True
        else:
            return False

    def combine(self, other):
        # If the other circle refers to the same as this circle,
        # add the points of other circle that are not in this circle.
        assert self.is_same_circle(other), "combine circle"
        if self.args != other.args:
            self.args = self.args.union(other.args)
        if other.center and not self.center:
            self.center = other.center


class Rule:
    """Represent a rule in geometry prover, e.g.:

    coll(A, C, B) :- coll(A, B, C) is
    Rule([coll(A, B, C)], coll(A, C, B))

    """
    def __init__(self, assums, concl):
        assert isinstance(assums, list) and all(isinstance(assum, Fact) for assum in assums)
        assert isinstance(concl, Fact)
        self.assums = assums
        self.concl = concl

    def __eq__(self, other):
        return isinstance(other, Rule) and self.assums == other.assums and self.concl == other.concl

    def __str__(self):
        return "%s :- %s" % (str(self.concl), ", ".join(str(assum) for assum in self.assums))


class MatchException(Exception):
    pass


def get_line(lines, pair):
    """Return a line from lines containing the given pair of points, if
    it exists. Otherwise return a line containing the pair.
    
    Examples:

    get_line([Line(P,Q,R)], (P, Q)) -> Line(P,Q,R)
    get_line([Line(P,Q,R)], (O, P)) -> Line(O,P)

    """
    assert isinstance(lines, list) and all(isinstance(line, Line) for line in lines)
    assert isinstance(pair, tuple) and len(pair) == 2 and all(isinstance(p, str) for p in pair)

    new_line = Line(list(pair))
    for line in lines:
        if line.is_same_line(new_line):
            return line

    return new_line


def get_circle(circles, points, center=None):
    """Return a circle from circles containing the given points and center (optional),
    if it exists. Otherwise return a circle containing the points and center (optional).

    """
    assert isinstance(circles, list) and all(isinstance(circle, Circle) for circle in circles)
    assert isinstance(points, list) and len(points) >= 3 and all(isinstance(p, str) for p in points)
    if center:
        assert isinstance(center, str)

    new_circle = Circle(points, center=center)
    for circle in circles:
        if new_circle.is_same_circle(circle):
            return circle

    return new_circle


def make_pairs(args, pair_len=2):
    """Divide input list args into groups of length pair_len (default 2)."""
    assert len(args) % pair_len == 0
    return [tuple(args[pair_len*i : pair_len*(i+1)]) for i in range(len(args) // pair_len)]


def match_expr(pat, f, inst, *, lines=None, circles=None, source=[]):
    """Match pattern with f, return a list of result(s).

    inst is a dictionary that assigns point variables to points,
    and line variables to pairs of points.

    lines: list of currently known lines.

    Multiple results will be generated if a line and two points on it
    need to be matched simultaneously.

    Example:

    match(coll(A, B, C), coll(P, Q, R), {}) -> [{A: P, B: Q, C: R}].
    match(coll(A, B, C), coll(P, Q, R), {A: P}) -> [{A: P, B: Q, C: R}].
    match(coll(A, B, C), coll(P, Q, R), {A: Q}) -> [].
    match(coll(A, B, C), para(P, Q, R, S), {}) -> [].

    match(perp(l, m), perp(P, Q, R, S), {}) -> [{l: (P, Q), m: (R, S)}]
    match(perp(l, m), perp(P, Q, R, S), {l: (Q, P)}) -> [{l: (Q, P), m: (R, S)}]
    match(perp(l, m), perp(P, Q, R, S), {l: (O, P)}, lines=[Line(O, P, Q)]) -> [{l: (O, P), m: (R, S)}]
    match(perp(l, m), perp(P, Q, R, S), {l: (O, P)}) -> [].

    """

    def c_process(flag):
        """Identical part of the processing for circ and cycl cases.
        
        flag -- whether the matching has already failed.

        """
        fixed = []  # arguments in pattern that are also in inst.
        same_args = list(set(pat.args).intersection(set(inst.keys())))
        for same_arg in same_args:
            if inst[same_arg] in c.args:
                fixed.append(same_arg)
            else:
                flag = True
        if not flag:  # start matching
            for_comb = sorted(list(c.args - set(inst.values())))
            if len(f.args) - len(fixed) > 0:
                # Order is not considered.
                comb = list(itertools.permutations(range(len(for_comb)), len(f.args) - len(fixed)))
                for c_nums in comb:
                    item = [for_comb[num] for num in c_nums]
                    p = 0
                    for i in range(len(pat.args)):
                        if pat.args[i] in fixed:
                            continue
                        inst[pat.args[i]] = item[p]
                        p += 1
                    new_insts.append(copy.copy(inst))
                    new_source = copy.copy(prev_source)
                    new_source.append(c_nums)
                    new_sources.append(new_source)
            else:  # remain previous insts and sources
                new_sources.append(prev_source)
                new_insts.append(inst)

    assert isinstance(pat, Fact) and isinstance(f, Fact)
    if lines is None:
        lines = []
    else:
        assert isinstance(lines, list) and all(isinstance(line, Line) for line in lines)

    if pat.pred_name != f.pred_name:
        return [], []

    arg_ty = get_arg_type_by_fact(pat)
    new_insts = []
    new_sources = []
    prev_source = copy.copy(source)

    if arg_ty == POINT:
        # coll or midp case
        # Generating all possible combinations from long fact:
        comb = list(itertools.combinations(range(len(f.args)), len(pat.args)))
        for c_nums in comb:
            c = [f.args[num] for num in c_nums]
            t_inst = copy.copy(inst)
            flag = False
            for p_arg, t_arg in zip(pat.args, c):
                if p_arg in t_inst:
                    if t_arg != t_inst[p_arg]:
                        flag = True
                else:
                    t_inst[p_arg] = t_arg
            if not flag:
                new_insts.append(t_inst)
                new_source = copy.copy(prev_source)
                new_source.append(c_nums)
                new_sources.append(new_source)

    elif arg_ty == LINE:
        # para, perp, or eqangle case, matching lines
        if f.pred_name == "eqangle":
            groups = make_pairs(f.args, pair_len=4)
            comb = list(itertools.combinations(range(len(groups)), 2))  # all possibilities
        else:
            groups = make_pairs(f.args)
            comb = list(itertools.permutations(range(len(groups)), len(pat.args)))

        for c_nums in comb:
            if f.pred_name == "eqangle":
                c = ((groups[c_nums[0]][0], groups[c_nums[0]][1]), (groups[c_nums[0]][2], groups[c_nums[0]][3]),
                     (groups[c_nums[1]][0], groups[c_nums[1]][1]), (groups[c_nums[1]][2], groups[c_nums[1]][3]))
            else:
                c = [groups[num] for num in c_nums]
            t_inst = copy.copy(inst)
            flag = False
            for p_arg, t_arg in zip(pat.args, c):
                if p_arg in t_inst:
                    l1 = get_line(lines, t_inst[p_arg])
                    l2 = get_line(lines, t_arg)
                    if l1 != l2:
                        flag = True
                else:
                    t_inst[p_arg] = t_arg
            if not flag:
                new_insts.append(t_inst)
                new_source = copy.copy(prev_source)
                new_source.append(c_nums)
                new_sources.append(new_source)

    elif arg_ty == SEG:
        # eqratio or cong case
        # Possible to assign t_inst[pat] to arg
        def can_assign(pat, arg):
            return pat not in t_inst or t_inst[pat] == arg

        new_insts = []
        groups = make_pairs(f.args)
        comb = list(itertools.combinations(range(len(groups)), len(pat.args) // 2))
        for c_nums in comb:
            c = [groups[num] for num in c_nums]
            t_insts = [copy.copy(inst)]
            for i in range(len(pat.args) // 2):
                ts = []
                for t_inst in t_insts:
                    pat_a, pat_b = pat.args[2*i : 2*i+2]
                    if can_assign(pat_a, c[i][0]) and can_assign(pat_b, c[i][1]):
                        t = copy.copy(t_inst)
                        t[pat_a] = c[i][0]
                        t[pat_b] = c[i][1]
                        ts.append(t)
                    if can_assign(pat_a, c[i][1]) and can_assign(pat_b, c[i][0]):
                        t = copy.copy(t_inst)
                        t[pat_a] = c[i][1]
                        t[pat_b] = c[i][0]
                        ts.append(t)
                t_insts = ts
            if t_insts:  # If have at least one new element generated
                new_source = copy.copy(prev_source)
                new_source.append(c_nums)
                for _ in range(len(t_insts)):
                    new_sources.append(new_source)
            new_insts.extend(t_insts)

    elif arg_ty == PonL:
        # para, perp, or eqangle, matching points
        # Generate possible lines selections (two lines in one selection).
        if f.pred_name == "eqangle":
            groups = make_pairs(f.args, pair_len=4)
            comb = list(itertools.combinations(range(len(groups)), len(pat.args) // 4))
        else:
            groups = make_pairs(f.args)
            comb = list(itertools.combinations(range(len(groups)), len(pat.args) // 2))
        base_inst = copy.copy(inst)

        for c_nums in comb:
            if f.pred_name == "eqangle":
                c = ((groups[c_nums[0]][0], groups[c_nums[0]][1]), (groups[c_nums[0]][2], groups[c_nums[0]][3]),
                     (groups[c_nums[1]][0], groups[c_nums[1]][1]), (groups[c_nums[1]][2], groups[c_nums[1]][3]))
            else:
                c = [groups[num] for num in c_nums]
            t_insts = [copy.copy(inst)]
            for i in range(len(pat.args) // 2):
                ts = []
                for t_inst in t_insts:
                    l = get_line(lines, c[i])
                    removed = copy.copy(l.args)
                    removed = removed - set(t_inst.values())
                    pat_a, pat_b = pat.args[i*2 : i*2+2]

                    if pat_a in t_inst:
                        if t_inst[pat_a] in l.args:
                            a = [t_inst[pat_a]]
                        else:
                            a = []
                    else:
                        a = removed

                    if pat_b in t_inst:
                        if t_inst[pat_b] in l.args:
                            b = [t_inst[pat_b]]
                        else:
                            b = []
                    else:
                        b = removed

                    perms = [[x, y] for x in a for y in b if x != y]
                    for a, b in perms:
                        t = copy.copy(t_inst)
                        t[pat_a], t[pat_b] = a, b
                        ts.append(t)
                t_insts = ts
            if t_insts:
                new_source = copy.copy(prev_source)
                new_source.append(c_nums)
                for _ in range(len(t_insts)):
                    new_sources.append(new_source)
            new_insts.extend(t_insts)  # t_insts is a empty list if nothing new generated

    elif arg_ty == CYCL:
        c = get_circle(circles, list(f.args))
        flag = False
        c_process(flag)

    elif arg_ty == CIRC:
        c = get_circle(circles, f.args[1:], f.args[0])
        flag = False
        if pat.args[0] in inst and inst[pat.args[0]] != f.args[0]:
            flag = True
        else:
            inst[pat.args[0]] = f.args[0]
        del f.args[0]
        del pat.args[0]
        c_process(flag)

    # TODO: Support more types.
    elif arg_ty == TRI:
        raise NotImplementedError
    else:
        raise NotImplementedError

    assert len(new_insts) == len(new_sources)
    return new_insts, new_sources


def make_new_lines(facts, lines):
    """Construct new lines from a list of given facts.

    The arguments of collinear facts will be used to construct new lines.
    Points in a new line will be merged into one of given lines,
    if the new line and the given line is the same line.
    The given list of lines will be updated.

    """
    assert isinstance(facts, list)
    assert all(isinstance(fact, Fact) for fact in facts)
    assert isinstance(lines, list)
    assert all(isinstance(line, Line) for line in lines)

    for fact in facts:
        if fact.pred_name == "coll":
            new_line = Line(fact.args)
            same = [l for l in lines if new_line.is_same_line(l)]
            for l in same:
                new_line.combine(l)
                lines.remove(l)
            lines.append(new_line)


def make_new_circles(facts, circles):
    """
    Construct new circles from a list of given facts.
    The arguments of cyclic and circle facts will be used to construct new circles.
    Points in a new line will be merged into one of given circles,
    if the new circle and the given circle is the same circle.
    The given list of circles will be updated.
    """
    assert isinstance(facts, list) and all(isinstance(fact, Fact) for fact in facts)
    assert isinstance(circles, list) and all(isinstance(circle, Circle) for circle in circles)

    for fact in facts:
        if fact.pred_name in ("cyclic", "circle"):
            if fact.pred_name == "cyclic":
                new_circle = Circle(list(fact.args))
            if fact.pred_name == "circle":
                new_circle = Circle(list(fact.args[1:]), center=fact.args[0])
            same = [c for c in circles if new_circle.is_same_circle(c)]
            for c in same:
                new_circle.combine(c)
                circles.remove(c)
            circles.append(new_circle)


def apply_rule(rule, facts, *, lines=None, circles=None, ruleset=None, hyps=None):
    """Apply given rule to the list of facts.

    If param facts is a list of integers: these integers represents the positions in hyps. In this case,
    hyps must be a list of facts. The new facts generated by this function will combine to hyps
    automatically. Function returns nothing.

    If param facts is a list of facts: New facts will be returned.

    Example:
    apply_rule(
        Rule([para(A, B, C, D), para(C, D, E, F)], para(A, B, E, F)),
             [para(P, Q, R, S), para(R, S, U, V)])
    -> para(P, Q, U, V).

    apply_rule(
        Rule([coll(A, B, C)], coll(A, C, B)),
             [para(A, B, D, C)])
    -> [].
    """
    def duplicate(e, length):
        """
        Get the condition of a new rule that generated by apply_rule() requires.
        length: how many characters are needed to form one single element in the fact. Decided by pred_name.
        Return cond for the new rule.
        """
        return [e] * (len(rule.concl.args) // length)

    assert isinstance(facts, list)
    assert all(isinstance(fact, int) for fact in facts)
    assert all(isinstance(fact, Fact) for fact in hyps)
    assert isinstance(ruleset, dict)
    assert isinstance(rule, str)

    rule_name = copy.copy(rule)
    rule = ruleset[rule]
    assert len(facts) == len(rule.assums)

    facts_pos = copy.copy(facts)
    facts = [hyps[i] for i in facts]

    insts = [dict()]
    sources = [[]]
    for assum, fact in zip(rule.assums, facts):  # match the arguments recursively
        new_insts = []
        new_sources = []
        for i in range(len(insts)):
            new, source = match_expr(assum, fact, insts[i], lines=lines, circles=circles, source=sources[i])
            new_insts.extend(new)
            new_sources.extend(source)
        insts = new_insts
        sources = new_sources

    for idx in range(len(insts)):  # An inst represents one matching result of match_expr().
        if rule.concl.args[0].islower():
            pos = duplicate(sources[idx], divisors_for_apply[rule.concl.pred_name] // 2)
            if facts_pos:
                cond = duplicate(facts_pos, divisors_for_apply[rule.concl.pred_name] // 2)
            concl_args = []
            for i in rule.concl.args:
                concl_args.extend((insts[idx][i][0], insts[idx][i][1]))
        else:
            pos = duplicate(sources[idx], divisors_for_apply[rule.concl.pred_name])
            if facts_pos:
                cond = duplicate(facts_pos, divisors_for_apply[rule.concl.pred_name])
            concl_args = [insts[idx][i] for i in rule.concl.args]

        new = Fact(rule.concl.pred_name, concl_args, updated=True, lemma=rule_name, cond=cond, pos=pos)
        combine_facts_list(new, hyps, lines, circles)


def combine_facts_list(fact, target, lines, circles):
    """
    Combine a fact with a list of facts (target).
    Use combine_facts() to combine every two facts in separate list.
    """
    assert isinstance(fact, Fact)
    for t in range(len(target)):
        r = combine_facts(fact, target[t], lines, circles)
        if r:
            target[t] = r
            return
    
    # If not combined: add to target
    target.append(fact)


def search_step(ruleset, hyps, only_updated=False, lines=None, circles=None):
    """One step of searching fixpoint.

    Apply given ruleset to a list of hypotheses to obtain new facts.
    If collinear facts are included in hypotheses, new lines can be
    automatically generated, these new lines might be used when
    applying rules to hypotheses.

    """
    # Update the list of lines.
    make_new_lines(hyps, lines)
    make_new_circles(hyps, circles)

    for rule_name, rule in ruleset.items():
        for seq in itertools.permutations(range(len(hyps)), len(rule.assums)):
            if not only_updated or any(hyps[i].updated for i in seq):
                apply_rule(rule_name, list(seq), lines=lines, circles=circles, ruleset=ruleset, hyps=hyps)


def search_fixpoint(ruleset, hyps, lines, circles, concl):
    """Recursively apply given ruleset to a list of hypotheses to
    obtain new facts. Recursion exits when new fact is not able
    to be generated, or conclusion is in the list of facts.
    Return the list of facts.
    """
    search_step(ruleset, hyps, lines=lines, circles=circles)
    prev_hyps = []
    prev_lines = []
    prev_circles = []
    while hyps != prev_hyps or lines != prev_lines or circles != prev_circles:
        prev_hyps = copy.copy(hyps)
        prev_lines = copy.copy(lines)
        prev_circles = copy.copy(circles)
        search_step(ruleset, hyps, only_updated=True, lines=lines, circles=circles)
        for i, fact in enumerate(hyps):
            if check_same(fact, concl, lines, circles):
                return i
    return hyps


def combine_facts(fact, goal, lines, circles):
    """
    Combine this fact to other fact.
    Return a combined long fact if succeed.

    """
    def get_pos(piece, long):
        """Check if piece is a segment of long.
        If it is, return its position in long. """
        pos = 0
        l = len(piece)
        if not long:
            return False
        if l == 1:
            while pos < len(long):
                if long[pos] == piece:
                    return pos
                pos += 1
        else:
            while pos < len(long):
                if long[pos * l: pos * (l + 1)] == piece:
                    return pos
                pos += 1
        return False

    def update_cond(new_list, pos):
        """Update cond of new_goal. """
        if not get_pos(new_list[pos], new_goal.args):
            if new_goal.cond:
                new_goal.cond.append(fact.cond[pos])
            else:
                new_goal.cond = fact.cond[pos]

    new_goal = copy.deepcopy(goal)

    if fact.pred_name != goal.pred_name:
        return False

    if fact.pred_name == "perp":
        fact_lines = [get_line(lines, (fact.args[0], fact.args[1])), get_line(lines, (fact.args[2], fact.args[3]))]
        goal_lines = [get_line(lines, (goal.args[0], goal.args[1])), get_line(lines, (goal.args[2], goal.args[3]))]
        if fact_lines == goal_lines:
            return goal
        return False

    elif fact.pred_name == 'coll':
        flg = False
        i = 0  # pointer of fact
        while i < len(fact.args):
            t_flg = False  # If identical part is detected in this recursion. Reset here.
            j = 0  # pointer of goal
            while j < len(goal.args):
                if fact.args[i] == goal.args[j]:
                    flg = True
                    t_flg = True
                    break
                j += 1
            if not t_flg:
                new_goal.cond.append(fact.cond[i])
                new_goal.pos.append(fact.pos[i])
                new_goal.args.append(fact.args[i])
            i += 1
        if not flg:
            return False
        return new_goal

    elif fact.pred_name in ('cyclic', 'circle'):
        if len(set(fact.args).intersection(set(goal.args))) >= 3:
            fact_args_lst = []
            for i in range(len(fact.args)):
                fact_args_lst.append((fact.args[i], i))
            new_pts = [i for i in fact_args_lst if i[0] not in goal.args]
            for p in range(len(new_pts)):
                if not get_pos(new_pts[p], new_goal.args):
                    new_goal.cond.append(fact.cond[new_pts[p][1]])
                    new_goal.pos.append(fact.pos[new_pts[p][1]])
                new_goal.args.append(new_pts[p][0])
            return new_goal
        else:
            return False

    elif fact.pred_name in ('eqratio', 'cong'):
        flg = False  # if identical part is detected in global
        i = 0  # pointer of fact
        while i < len(fact.args):
            t_flg = False  # If identical part is detected in this recursion
            j = 0  # pointer of goal
            while j < len(goal.args):
                if (fact.args[i] == goal.args[j] and fact.args[i + 1] == goal.args[j + 1]) or \
                        (fact.args[i] == goal.args[j + 1] and fact.args[i + 1] == goal.args[j]):  # exchange available
                    flg = True
                    t_flg = True
                    break
                j += 2
            if not t_flg:  # add not identical part (4 pts) to new_goal
                new_goal.cond.append(fact.cond[int(i / 2)])
                new_goal.pos.append(fact.pos[int(i / 2)])
                new_goal.args.extend((fact.args[i], fact.args[i + 1]))
            i += 2

        if not flg:  # return False if no identical part at all
            return False
        return new_goal

    elif fact.pred_name == 'para':
        # Two facts that have the pred_name "para" can be combined
        # if they have at least one identical line.
        # Get all lines in fact and goal.
        fact_pts, goal_pts = make_pairs(fact.args), make_pairs(goal.args)
        fact_lines = [get_line(lines, pair) for pair in fact_pts]
        goal_lines = [get_line(lines, pair) for pair in goal_pts]

        flg = False
        # print(len(fact_lines))
        for idx, fact_line in enumerate(fact_lines):
            t_flg = False
            for goal_line in goal_lines:
                if fact_line.is_same_line(goal_line):
                    flg = True
                    t_flg = True
                    break
            if not t_flg:  # add not identical part (2 pts) to new_goal
                new_goal.cond.append(fact.cond[idx])
                # print(fact.pos)
                new_goal.pos.append(fact.pos[idx])
                new_goal.args.extend(fact_pts[idx])
        if not flg:
            return False
        return new_goal

    elif fact.pred_name == "eqangle":
        fact_pts, goal_pts = make_pairs(fact.args), make_pairs(goal.args)
        fact_lines = [get_line(lines, pair) for pair in fact_pts]
        goal_lines = [get_line(lines, pair) for pair in goal_pts]

        flg = False
        i = 0
        while i < len(fact_lines):
            t_flg = False
            flg2 = False
            j = 0
            while j < len(goal_lines):
                if fact_lines[i].is_same_line(goal_lines[j]) and fact_lines[i + 1].is_same_line(goal_lines[j + 1]):
                    flg = True
                    t_flg = True
                    break
                j += 2
            if not t_flg:  # add not identical part (4 pts) to new_goal
                pos = 0
                while pos < len(new_goal.args):
                    if (fact_pts[i][0] == new_goal.args[pos] and fact_pts[i][1] == new_goal.args[pos + 1] and
                            fact_pts[i + 1][0] == new_goal.args[pos + 2] and fact_pts[i + 1][1] == new_goal.args[
                                pos + 3]):
                        flg2 = True
                        break
                    pos += 4
                if not flg2:
                    new_goal.cond.append(fact.cond[int(i / 2)])
                    new_goal.pos.append(fact.pos[int(i / 2)])
                new_goal.args.extend(fact_pts[i])
                new_goal.args.extend(fact_pts[i + 1])
            i += 2
        if not flg:
            return False
        return new_goal
    else:
        if fact.args == goal.args:
            return goal
        return False


def check_same(fact, goal, lines, circles):
    """Check if the given fact refers to a same fact as goal."""
    if fact.pred_name != goal.pred_name:
        return False

    if fact.pred_name == "perp":
        # Check the two lines are the same
        fact_lines = [get_line(lines, (fact.args[0], fact.args[1])), get_line(lines, (fact.args[2], fact.args[3]))]
        goal_lines = [get_line(lines, (goal.args[0], goal.args[1])), get_line(lines, (goal.args[2], goal.args[3]))]
        return fact_lines == goal_lines
    elif fact.pred_name == 'coll':
        # Whether they are the same line
        fact_pts, goal_pts = set(fact.args), set(goal.args)
        return len(fact_pts) + len(goal_pts) - len(goal_pts.intersection(fact_pts)) >= 2
    elif fact.pred_name == 'circle':
        # Whether they are the same circle (with center)
        fact_circle = get_circle(circles, fact.args[1:], center=fact.args[0])
        goal_circle = get_circle(circles, goal.args[1:], center=goal.args[0])
        return fact_circle.is_same_circle(goal_circle)
    elif fact.pred_name == 'cyclic':
        # Whether they are the same circle
        fact_circle = get_circle(circles, list(fact.args))
        goal_circle = get_circle(circles, list(goal.args))
        return fact_circle.is_same_circle(goal_circle)
    elif fact.pred_name in ('eqratio', 'cong'):
        # Check whether fact and goal share a pair of points (exchange possible)
        for i in range(len(fact.args) // 2):
            for j in range(len(goal.args) // 2):
                if (fact.args[2*i] == goal.args[2*j] and fact.args[2*i+1] == goal.args[2*j+1]) or \
                   (fact.args[2*i] == goal.args[2*j+1] and fact.args[2*i+1] == goal.args[2*j]):
                    return True
        return False
    elif fact.pred_name == 'para':
        # Check whether fact and goal share a line
        fact_pts, goal_pts = make_pairs(fact.args), make_pairs(goal.args)
        fact_lines = [get_line(lines, pair) for pair in fact_pts]
        goal_lines = [get_line(lines, pair) for pair in goal_pts]
        return any(l1.is_same_line(l2) for l1 in fact_lines for l2 in goal_lines)
    elif fact.pred_name == "eqangle":
        # Check whether fact and goal share a pair of lines
        fact_pts, goal_pts = make_pairs(fact.args), make_pairs(goal.args)
        fact_lines = [get_line(lines, pair) for pair in fact_pts]
        goal_lines = [get_line(lines, pair) for pair in goal_pts]
        for i in range(len(fact_lines) // 2):
            for j in range(len(goal_lines) // 2):
                if fact_lines[2*i].is_same_line(goal_lines[2*j]) and fact_lines[2*i+1].is_same_line(goal_lines[2*j+1]):
                    return True
        return False


separators = {
    "eqangle": " = ",
    "default": ","
}


def print_search(ruleset, facts, concl):
    """Print the process of searching fixpoint.
    The given list of facts must contains all the deduce procedures
    (as parameters of facts in the list). Using a given ruleset to
    find out the name of rules used in deduce procedures.
    """

    def write_fact_content(name, args):
        """Generate more explicit form of given fact in terms of printing (if possible). """
        if name == "eqangle":
            s = ""
            i = 0
            while i < len(args):
                s = s + "∠[" + args[i] + args[i + 1] + "," + args[i + 2] + args[i + 3] + "] = "
                i += 4
            return s[:-3]
        else:
            return ",".join(args)

    def write_fact_outline(name, s):
        if name == "eqangle":
            return s
        else:
            return name + "(" + s + ")"

    def print_step(fact, sub_pos):
        sep = separators[fact.pred_name] if fact.pred_name in separators.keys() else separators["default"]
        d = divisors_for_apply[fact.pred_name] if divisors_for_apply[fact.pred_name] != 0 else len(fact.args)
        msg = "(" + str(ruleset[fact.lemma]) + ") " + write_fact_outline(fact.pred_name,
        sep.join([write_fact_content(fact.pred_name, fact.args[pos * d: (pos + 1) * d]) for pos in sub_pos])) + " :- "
        need_expand = dict()
        for elem_pos in sub_pos:
            for c, p in zip(fact.cond[elem_pos], fact.pos[elem_pos]):
                if c in need_expand.keys():
                    need_expand[c].update(p)
                else:
                    need_expand[c] = set(p)
        for sub_fact_id in need_expand.keys():
            sub_fact = facts[sub_fact_id]
            name = sub_fact.pred_name
            sep = separators[name] if name in separators.keys() else separators["default"]
            if sub_fact.updated:
                print_step(sub_fact, need_expand[sub_fact_id])
            d = divisors_for_apply[name] if divisors_for_apply[name] != 0 else len(sub_fact.args)
            single_cond_msg = sep.join(
                [write_fact_content(name, sub_fact.args[pos * d: (pos + 1) * d]) for pos in need_expand[sub_fact_id]])
            msg = msg + write_fact_outline(name, single_cond_msg) + ", "
        print(msg[:-2])

    # Get started from all sub-parts of the concl.
    print_step(concl, range(len(concl.args) // divisors_for_apply[concl.pred_name]))