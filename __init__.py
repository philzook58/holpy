import sys, os

# janky patch to enable relative imports
submodule_root = os.path.dirname(__file__)
if submodule_root not in sys.path:
    sys.path.append(submodule_root)
