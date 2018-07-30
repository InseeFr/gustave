# Shell script to check an R package
# Inspired from https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/script/r.rb

# Install R dependencies
R -e "devtools::install_deps('.', c('Depends', 'Imports', 'Suggests'))"

# Build and check package
R CMD build . $r_build_args
PKG_FILE_NAME=$(ls -1t *.tar.gz | head -n 1)
R CMD check "${PKG_FILE_NAME}" $r_check_args

# Detect warnings and errors
RCHECK_DIR=$(ls -1t *.tar.gz | head -n 1 | cut -d'_' -f 1).Rcheck
cd $RCHECK_DIR
number_error=$(grep ERROR 00check.log | wc -l)
number_warning=$(grep WARNING 00check.log | wc -l)
if [ $number_error -ne 0 ]; then { echo "Found errors in R CMD check."; exit 1; }; fi;
if [ $warnings_are_errors -eq "1" ] && [ $number_warning -ne 0 ]; then { echo "Found warnings in R CMD check, treating as errors (as requested)."; exit 1; }; fi;
