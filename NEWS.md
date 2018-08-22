# 0.4.0

- Breaking: Heavy remanufacturing of `define_variance_wrapper`

    * New: `technical_data` argument offers a more consistent way to include technical data within the enclosing environment of the wrapper. `objects_to_include` is kept for non-data objects (such as additional statistic wrappers) or advanced customization.
    * New: `technical_param` argument offers a more convenient way to specify default values for parameters used by the variance function.
    * New: `reference_weight` replaces `default$weight`. This means that the reference weight used for point estimation and linearization is set while defining the variance wrapper and not at run-time.
    * Deprecated: `stat`, which was a remain of an early implementation of linearization functions, is not a parameter of the variance wrappers anymore. Its purpose (to apply a given variance wrapper to several variables without having to type the name of the linearization wrapper) is now covered by the standard evaluation capabilities of statistic wrappers (see below).
    * Deprecated: `default` is replaced by `default_id`, as `default$weight` and `default$stat` are no longer needed. As for `default$alpha`, its value is set to 0.05 and cannot be changed anymore while defining the variance wrapper (as this can easily be done afterwards using `formals<-`).
    * Deprecated: `objects_to_include_from`

- Breaking: Rebranding and heavy remanufacturing of `define_statistic_wrapper` (previously known as `define_linearization_wrapper`), added support for standard evaluation (see `define_variance_wrapper` examples).
    
- New: the `qvar` function allows for a straigthforward variance estimation in common cases (stratified simple random sampling with non-response through reweighting and calibration) and performs both technical and methodological checks.

- Some normalization in function names: `add0` becomes `add_zero`, `sumby` becomes `sum_by`, `rescal` becomes `res_cal`

- Example data: calibration variables in ict_sample instead of ict_survey


# 0.3.1

- Hotfix: Add calibrated weights to `define_variance_wrapper` example.

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

