"""
Script for running a folder of smt files as well as store the stastics in a .csv file.

Usage: python -m stat FOLDER_NAME
"""
import time
from datetime import datetime
import os
import sys
import csv
import concurrent.futures
from itertools import repeat
import functools
import errno

from smt.veriT import interface, proof_rec, proof_parser
from smt.veriT.verit_macro import VeriTException
from syntax.settings import settings
settings.unicode = False

sys.setrecursionlimit(10000)

smtlib_path = None

try:
    with open('smt/veriT/tests/smtlib_path.txt', 'r') as f:
        smtlib_path = f.readline().strip()
except FileNotFoundError:
    print("File smtlib_path.txt should be present in the smt/veriT/tests/ directory,")
    print("containing the path to the smtlib folder.")

import signal

class TO:
    def __init__(self, seconds=1, error_message='Timeout'):
        if seconds <= 1:
            self.seconds =  1
        else:
            self.seconds = int(seconds)
        self.error_message = error_message
    def handle_timeout(self, signum, frame):
        raise TimeoutError(self.error_message)
    def __enter__(self):
        signal.signal(signal.SIGALRM, self.handle_timeout)
        signal.alarm(self.seconds)
    def __exit__(self, type, value, traceback):
        signal.alarm(0)

class TimeoutError(Exception):
    pass

def timeout(seconds=10, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)

        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return wrapper

    return decorator


try:
    with open('smt/veriT/tests/smtlib_path.txt', 'r') as f:
        smtlib_path = f.readline().strip()
except FileNotFoundError:
    print("File smtlib_path.txt should be present in the smt/veriT/tests/ directory,")
    print("containing the path to the smtlib folder.")

def test_parse_step(verit_proof, ctx):
    parser = proof_parser.proof_parser(ctx)
    steps = []
    for s in verit_proof.replace("\r", "").split("\n"):
        if s == "unsat" or s == "":
            continue
        steps.append(parser.parse(s))

    return steps


def test_proof(filename, solve_timeout=120):
    """note: sys.getsizeof(verit_proof) failed in pypy3, so we can't get the proof size now"""
    print(filename)
    unsat, res = interface.is_unsat(filename, timeout=solve_timeout)
    if not unsat:
        return [filename[11:], 'UN-UNSAT']
    verit_proof = interface.solve(filename, timeout=solve_timeout)
    if verit_proof is None:
        return [filename[11:], 'NO PROOF']
    else:
        return [filename[11:], "RETURN PROOF"]

def test_file(filename, show_time=True, test_eval=False, test_proofterm=False,
              step_limit=None, omit_proofterm=None, solve_timeout=120, eval_timeout=300):
    """Test a given file under eval or proofterm mode."""
    stastic = []
    global smtlib_path
    if not smtlib_path:
        return

    if filename[-4:] != 'smt2':
        return
    stastic.append(filename)
    unsat, res = interface.is_unsat(filename, timeout=solve_timeout)
    if not unsat:
        return [filename, str(res), '', '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    print(repr(filename) + ',')

    # assts = proof_rec.get_assertions(filename) 

    # Solve
    start_time = time.perf_counter()
    verit_proof = interface.solve(filename, timeout=solve_timeout)
    if verit_proof is None:
        print([filename, 'NO PROOF (veriT)', '', '', ''])
        return [filename, 'NO PROOF (veriT)', '', '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    if verit_proof.strip() == "unknown":
        print("%s unknown proof" % filename)
        return [filename, 'UNKNOWN PROOF (veriT)', '', '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    solve_time = time.perf_counter() - start_time
    solve_time_str = "%.3f" % solve_time


    total_time = 60
    start_time = time.perf_counter()
    try:
        with TO(seconds=total_time):
            try:
                ctx = proof_rec.bind_var(filename)
            except Exception as e:
                print([filename, solve_time_str, "BIND_VAR %s (HolPy)" % str(e), '', ''])
                return [filename, solve_time_str, "BIND_VAR %s (HolPy)" % str(e), '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    except Exception as e:
        print([filename, solve_time_str, "PARSER ERROR %s" % str(e), '', ''])
        return [filename, solve_time_str, "PARSER ERROR %s" % str(e), '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    end_time = time.perf_counter()
    total_time -= (end_time - start_time)
    if total_time < 0:
        print([filename, solve_time_str, "TIMEOUT", "TIMEOUT", '', ''])
        return [filename, solve_time_str, "TIMEOUT", "TIMEOUT", '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    # Parse
    start_time  = time.perf_counter()
    try: # timeout error
        with TO(seconds=total_time):
            try: # parsing error
                steps = test_parse_step(verit_proof, ctx)
            except Exception as e:
                print([filename, solve_time_str, 'PARSING ERROR (HolPy) %s' % e, '', ''])
                return [filename, solve_time_str, 'PARSING ERROR (HolPy) %s' % e, '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    except Exception as e: # should consider timeout error as well as other unexpected error
        print("%s PARSING TIMEOUT %s" % (filename, str(e)))
        return [filename, solve_time_str, 'PARSING ERROR %s (HolPy)' % e, '', '']
    parse_time = time.perf_counter() - start_time
    parse_time_str = "%.3f" % parse_time
    if parse_time > total_time:
        print([filename, solve_time_str, 'PARSING TIMEOUT (HolPy)', '', ''])
        return [filename, solve_time_str, 'PARSING TIMEOUT (HolPy)', '', '', datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
    total_time -= parse_time
    # Validation by macro.eval
    eval_time_str = ""
    if test_eval:
        start_time = time.perf_counter()
        recon = proof_rec.ProofReconstruction(steps)
        try:
            with TO(seconds=total_time):
                try:
                    pt = recon.validate(is_eval=True, step_limit=step_limit, omit_proofterm=omit_proofterm, with_bar=False)
                except Exception as e:
                    print([filename, solve_time_str, parse_time_str, 'Filename: %s Error: %s' % (str(filename), str(e)), len(steps)])
                    return  [filename, solve_time_str, parse_time_str, 'Filename: %s Error: %s' % (str(filename), str(e)), len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]                   
        except TimeoutError:
            print([filename, solve_time_str, parse_time_str, 'Proof evaluation is timeout! (HolPy)', len(steps)])
            return [filename, solve_time_str, parse_time_str, 'Proof evaluation is timeout! (HolPy)', len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
        eval_time = time.perf_counter() - start_time
        eval_time_str = "Eval: %.3f" % eval_time
        assert pt.rule != "sorry"
        print([filename, solve_time_str, parse_time_str, eval_time_str, len(steps)])
        return [filename, solve_time_str, parse_time_str, eval_time_str, len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]

    # Validation by macro.get_proof_term
    proofterm_time_str = ""
    if test_proofterm:
        start_time = time.perf_counter()
        recon = proof_rec.ProofReconstruction(steps)
        try:
            with TO(seconds=300):
                try:
                    print("TOTAL TIME %.3f" % total_time)
                    pt = recon.validate(is_eval=False, step_limit=step_limit, omit_proofterm=omit_proofterm, with_bar=False)
                    assert pt.rule != "sorry"
                except Exception as e:
                    print([filename, solve_time_str, parse_time_str, 'Error: %s %s %s' % (str(filename), str(e), time.perf_counter() - start_time), len(steps)])
                    return [filename, solve_time_str, parse_time_str, 'Error: %s %s %s' % (str(filename), str(e), time.perf_counter() - start_time), len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
        except (RecursionError,Exception) as e: # maybe other error?
            print("%s proof reconstruction timeout %s" % (filename, str(e)))
            if isinstance(e, TimeoutError):
                print([filename, solve_time_str, parse_time_str, 'Proof reconstruction is timeout! (HolPy)', len(steps)])
                return [filename, solve_time_str, parse_time_str, 'Proof reconstruction is timeout! (HolPy)', len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
            else:
                print([filename, solve_time_str, parse_time_str, 'Proof reconstruction failed %s' % str(e), len(steps)])
                return [filename, solve_time_str, parse_time_str, 'Proof reconstruction failed %s' % str(e), len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
        proofterm_time = time.perf_counter() - start_time
        proofterm_time_str = "Proofterm: %.3f" % proofterm_time

        print([filename, solve_time_str, parse_time_str, proofterm_time_str, len(steps)])
        return [filename, solve_time_str, parse_time_str, proofterm_time_str, len(steps), datetime.now().strftime('%Y-%m-%d %H:%M:%S')]

def test_path(path, show_time=True, test_eval=False, test_proofterm=False,
              step_limit=None, omit_proofterm=None, solve_timeout=10, eval_timeout=120):
    """Test a directory containing SMT files.
    
    test_eval : bool - test evaluation of steps.
    test_proofterm : bool - test proof term reconstruction of steps.
    step_limit : [None, int] - limit on number of steps to test for each file.
    omit_proofterm : List[str] - list of macro names for which proof term reconstruction
        is omitted (evaluation is used instead).
        
    """
    global smtlib_path
    if not smtlib_path:
        return

    abs_path = smtlib_path + path

    stats = []

    if path != "" and not os.path.exists(abs_path):
        print("Directory %s not found." % path)
        return

    if path == "":
        print("test full")
        with open("./smt/veriT/data/test_files.txt") as f:
            file_names = [smtlib_path+file_name[:-1] for file_name in f.readlines()]
    elif os.path.isfile("./smt/veriT/data/%s.csv" % path):
        file_names = []
        with open("./smt/veriT/data/%s.csv" % path) as f:
            f_csv = csv.reader(f)
            headers = next(f_csv)
            for row in f_csv:
                if len(row) == 2 and "RETURN PROOF" == row[-1]:
                    file_names.append(smtlib_path+row[0])
    else:
        _, file_names = run_fast_scandir(abs_path, ['.smt2'])
    if len(file_names) > os.cpu_count():
        max_workers = os.cpu_count()
    else:
        max_workers = len(file_names)
    print("start at %s" %  datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
    with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
        res = executor.map(test_file, file_names, repeat(show_time),
                        repeat(test_eval), repeat(test_proofterm), repeat(step_limit),
                            repeat(omit_proofterm), repeat(solve_timeout), repeat(eval_timeout))
    print("end")
    return res

def run_fast_scandir(dir, ext):    # dir: str, ext: list
    subfolders, files = [], []

    for f in os.scandir(dir):
        if f.is_dir():
            subfolders.append(f.path)
        if f.is_file():
            if os.path.splitext(f.name)[1].lower() in ext:
                files.append(f.path)


    for dir in list(subfolders):
        sf, f = run_fast_scandir(dir, ext)
        subfolders.extend(sf)
        files.extend(f)
    return subfolders, files

def test_path_proof(path, solve_timeout=120):
    global smtlib_path
    if not smtlib_path:
        return

    abs_path = smtlib_path + path

    stats = []

    if not os.path.exists(abs_path):
        print("Directory %s not found." % path)
        return

    _, file_names = run_fast_scandir(abs_path, ['.smt2'])
    time1 = time.perf_counter()
    if len(file_names) > os.cpu_count():
        max_workers = os.cpu_count()
    else:
        max_workers = len(file_names)
    with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
        res = executor.map(test_proof, file_names, repeat(solve_timeout))
    time2 = time.perf_counter()
    csv_name = path.replace('/', '.')
    if not os.path.isdir("./smt/veriT/data"):
        os.mkdir("./smt/veriT/data")
    file_name = "./smt/veriT/data/%s.csv" % csv_name
    with open(file_name, 'w') as f:
        f_csv = csv.writer(f)
        f_csv.writerow(['FILENAME', 'STATUS'])
        f_csv.writerows(res)
        f_csv.writerow(["TOTAL TIME %.3f" % (time2 - time1)])
        f_csv.writerow(["TIMESTAMP %s" % datetime.now().strftime('%Y-%m-%d %H:%M:%S')])
    return res



# Parameters
# 1. folder name
# 2. eval (--eval) or get_proof_term (--proofterm)
# 2. verit solve timeout (default: 10s)
# 3. eval timeout (default: 180s)
if __name__ == "__main__":
    folder_name = str(sys.argv[1])
    solve_timeout = 120
    eval_timeout  = 300
    test_eval = True # test eval as default, test proofterm if it is false
    find_proof = False
    test_full = False
    if len(sys.argv) == 3:
        if sys.argv[2] == "--proofterm":
            test_eval = False
        elif sys.argv[2] == "--eval":
            test_eval = True
        elif sys.argv[2] == "--find-proof":
            find_proof = True
    if sys.argv[1] == "--SMT-LIB":
        test_full = True
        assert len(sys.argv) == 3
        last_arg = sys.argv[-1]
        assert last_arg in ("--eval", "--proofterm")
        if last_arg == "--eval":
            test_eval = True
        else:
            test_eval = False
    print()
    for arg in sys.argv:
        print(arg)
    print("test_full %s" % test_full)
    print("folder")
    start_time = time.perf_counter()
    if find_proof:
        test_path_proof(folder_name, solve_timeout=120)
    elif test_full:
        stats = test_path("", test_eval=test_eval, test_proofterm=not test_eval, solve_timeout=solve_timeout, eval_timeout=eval_timeout)
    elif not test_full:
        if len(sys.argv) == 4:
            solve_timeout = int(sys.argv[3])
        elif len(sys.argv) == 5:
            solve_timeout = int(sys.argv[3])
            eval_timeout  = int(sys.argv[4])
        
        start_time = time.perf_counter()
        if test_eval:
            stats = test_path(folder_name, test_eval=True, test_proofterm=False, solve_timeout=solve_timeout, eval_timeout=eval_timeout)
        else:
            stats = test_path(folder_name, test_eval=False, test_proofterm=True, solve_timeout=solve_timeout, eval_timeout=eval_timeout)
    
    end_time = time.perf_counter()
    print("stats", stats)
    if not os.path.isdir('./smt/veriT/stastics'):
        os.mkdir('./smt/veriT/stastics')
    if test_eval and not os.path.isdir('./smt/veriT/stastics/eval'):
        os.mkdir('./smt/veriT/stastics/eval')
    if not test_eval and not os.path.isdir('./smt/veriT/stastics/proofterm'):
        os.mkdir('./smt/veriT/stastics/proofterm')
    
    if test_full:
        csv_name = "SMT-LIB"
    else:
        csv_name = folder_name.replace('/', '.')
    
    if test_eval:
        headers = ['filename', 'Solve', 'Parse', 'Eval', 'Steps']
        res_file_name = './smt/veriT/stastics/eval/%s.csv' % csv_name
    else:
        headers = ['filename', 'Solve', 'Parse', 'ProofTerm', 'Steps']
        res_file_name = './smt/veriT/stastics/proofterm/%s.csv' % csv_name

    with open(res_file_name, 'w') as f:
        f_csv = csv.writer(f)
        f_csv.writerow(headers)
        f_csv.writerows(stats)
        # TODO: record the MAXRECURSION
        if test_eval:
            f_csv.writerow(['Solve timeout: %s' % solve_timeout, 'Eval timeout: %s' % eval_timeout])
        else:
            f_csv.writerow(['Solve timeout: %s' % solve_timeout, 'ProofTerm timeout: %s' % eval_timeout])
        f_csv.writerow(["Total time: %.3f" % (end_time - start_time)])
        f_csv.writerow([datetime.now().strftime('%Y-%m-%d %H:%M:%S')])