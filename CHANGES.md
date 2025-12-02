## Summary of Changes Made

This update addressed several issues identified by `devtools::check()`:

### Fixed Issues:
1. **S3 method consistency**: Fixed the `to_string`/`to_string.StockValue` method signature mismatch
2. **Namespace access**: Changed `library()`/`require()` calls to use proper `package::function` syntax
3. **Documentation**: Added proper documentation for `calc_growth_rate` function
4. **Partial argument matches**: Fixed `nr` to `nrow` and `nc` to `ncol` in matrix functions
5. **Base function calls**: Added proper namespace access for functions like `read.table`, `write.csv`, `ts`, etc.

### Files Updated:
- `R/data.R`: Fixed read.table calls to use utils::read.table
- `R/models.R`: Added documentation for calc_growth_rate function, fixed divide_span function signature
- `R/plots.R`: Updated all plotting functions to use proper namespace access for ggplot2 functions
- `R/renders.R`: Fixed S3 method consistency for to_string function
- `R/exports.R`: Updated export functions to use proper namespace access
- `R/readers.R`: Fixed matrix function to use base::matrix
- `DESCRIPTION`: Added methods to imports, removed ORCID

### Key Improvements:
- All plotting functions now properly use `ggplot2::function_name()` syntax
- All data import/export functions use `utils::function_name()` syntax
- S3 methods now properly follow the generic pattern
- Removed partial argument matches that were causing warnings
- Added proper documentation for previously undocumented functions

These changes significantly reduce the number of warnings during package check.