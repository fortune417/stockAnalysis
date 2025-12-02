## Unit Tests for stockAnalysis Package

This directory contains unit tests for the stockAnalysis package, created to ensure the reliability and correctness of the package functions.

### Test Files:
- `test_functions.R`: Core function tests covering:
  - SEC data processing functions (read_sec_sub, read_sec_tag, read_sec_num, etc.)
  - Financial analysis functions (calc_growth_rates, dcf, calculate_ratios, etc.)
  - Utility functions (matrix_to_vec, reduce_digits, etc.)
  - Stock evaluation functions (find_undervalued_stocks, add_valuation_score, etc.)

- `test_additional_functions.R`: Additional comprehensive tests covering:
  - Statistical modeling functions (robust_lm)
  - Time series creation (create_financial_ts)
  - Stock screening (stock_screen)
  - Valuation ranking (rank_stocks_by_valuation)
  - Export functionality (export_to_csv and related functions)
  - Data merging (merge_sec_data)
  - Ratio calculations (calculate_ratios)

### Testing Approach:
- Each test validates specific functionality with appropriate test data
- Tests handle edge cases and error conditions appropriately
- Complex functions with external dependencies (plotting) are appropriately skipped
- All tests pass without errors or warnings (except for 2 skipped tests due to complex dependencies)

### Coverage:
The tests provide good coverage of the main functionality in the package:
- Data processing and ingestion functions
- Financial analysis and modeling functions
- Valuation and screening functions
- Utility and export functions
- S3 methods and object handling

### Notes:
- Plotting functions are excluded from testing due to complex ggplot2 dependencies in test environments
- Generic `to_string` function is skipped as it only has specific methods implemented
- Functions that depend on external data sources are tested with simulated data