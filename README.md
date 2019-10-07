## holpy

Implementation of higher-order logic in Python.

### Installation and usage:

This project requires Python 3.5.

Required packages are listed in requirements.txt. To install required packages, use:

```python -m pip install -r requirements.txt```

The current user interface is built using Vue, in the ./app folder. To start,
change to ./app and use ```npm install``` followed by ```npm run serve```,
then start the server (in the root directory) using ```python app.py```,
and go to page ```localhost:8080```.

To see a stable version of the old user interface, use:

```git checkout icfem```

Then start the server using ```python app.py```, and go to page
```http://127.0.0.1:5000/master```.

To see statistics for the search functionality on a collection of lemmas, use:

```python -m unittest server.tests.collect_stat```

Unit tests for the backend are located in files of the form ```*/tests/*_test.py```.

### Directory structure:

* [`kernel`](kernel/): kernel for higher-order logic.
  * [`type`](kernel/type.py): datatype for HOL types.
  * [`term`](kernel/term.py): datatype for HOL terms.
  * [`thm`](kernel/thm.py): datatype for HOL theorems, including list of basic derivations.
  * [`proof`](kernel/proof.py): linear representation of a proof, consisting of a list of instructions to the kernel.
  * [`macro`](kernel/macro.py): macros as user-defined proof methods.
  * [`theory`](kernel/theory.py): theory state, containing signature for types and constants, and list of axioms and theorems.
  * [`extension`](kernel/extension.py): types of extensions to a theory.
  * [`report`](kernel/report.py): statistics and debugging information for checking a theory extension.

* [`logic`](logic/): base logic and standard automation.
  * [`matcher`](logic/matcher.py): matching of terms.
  * [`proofterm`](logic/proofterm.py): tree-like representation of a proof. Used for convenient construction of proofs, and can be transformed to the linear representation.
  * [`conv`](logic/conv.py): conversions.
  * [`logic`](logic/logic.py): utilities and definition of standard macros for logic.
  * [`induct`](logic/induct.py): definition of types and constants by induction.
  * [`basic`](logic/basic.py): functions for loading theories.

* [`data`](data/): common data types.
  * [`nat`](data/nat.py): natural numbers.
  * [`function`](data/function.py): functions.
  * [`list`](data/list.py): lists.
  * [`set`](data/set.py): sets.
  * [`real`](data/real.py): real numbers.

* [`syntax`](syntax/): parsing and printing.
  * [`operator`](syntax/operator.py): data for unary and binary operators.
  * [`infertype`](syntax/infertype.py): type inference.
  * [`printer`](syntax/printer.py): printing functions.
  * [`parser`](syntax/parser.py): parsing functions, built using Lark parser.

* [`server`](server/): toplevel functions.
  * [`server`](server/server.py): definition and standard operations for proof states.
  * [`tactic`](server/tactic.py): definition of tactics.
  * [`method`](server/method.py): definition of methods.

* [`app`](app/): web application.
  * [`__init__.py`](app/__init__.py): main server program.

* [`library`](library/): main library of theories.
