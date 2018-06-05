
# 0.3.0

- Simulated data added
- Significant increase of unit tests
- Documentation completed
- Simplification of the structure of the main object processed by the variance wrapper
- Removal of unnecessary arguments in linearization wrappers
- Removal of the linerization wrappers for the Laeken indicators based on the vardpoor package (better integration in a future release)
- Preparation for a first CRAN release

# 0.2.7

- Now linearization with all data parameters set to NULL are discarded from the estimation.

# 0.2.6 

- Bug fix: evaluation of variables can occur either in the data argument or in the evaluation environment (envir argument)

# 0.2.3-0.2.5

- Several attempts to output more metadata from linearization functions. 
- At the end : ratio() gains two metadata slots, est_num and est_denom

# 0.2.2

- Minor bug fixes

# 0.2.1

- Beginning of the documentation
- Renaming of numerous functions and arguments
- Change the precalc structure in varDT
- Normalize the treatment of weights
- New linearization wrappers: gini() and arpr()

# 0.1.3-0.1.7

- No more dependency to package pryr
- Add the generalized inverse in `varDT`
- Other bug fixes

