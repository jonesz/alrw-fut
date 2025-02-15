# Core
* Compute `sigma` correctly. I believe `sigma` corresponds to something similar to the
"bandwidth" parameter in KDEs.

# Documentation
* Outright define what `z`, `sigma`, the conformal score cutoff value, etc. refer to.

# Testing
* Construct a set of functions, a framework for validating prediction sets; i.e. I have
some value `y`, does it exist within our prediction set?
* Construct utilities for evaluating the efficiency of the prediction set size; this will
likely have to be a monte carlo approach for determining the size of a set of overlapping
spheres.
