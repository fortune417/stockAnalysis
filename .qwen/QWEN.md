# Goal

This is an R package repository to accomplishe the following goals:

- process the downloaded SEC data https://www.sec.gov/data-research/sec-markets-data/financial-statement-data-sets, example data are in the the folder SEC/
- read the processed data into R data frames
- merge the data filed across different dates and different forms (10-K, 10-Q, etc.), so that for each company we have a time series of financial statements
- provide functions to analyze the financial statements data in R, computing intrinsic values, financial ratios, etc.
- provide functions to visualize the financial statements data in R, plotting trends, comparisons, etc.
- provide functions to export the financial statements data and analysis results to various formats, such as CSV, Excel, PDF, etc.
- provide functions to update the SEC data periodically, downloading the latest data and reprocessing it
- provide documentation and examples on how to use the package functions effectively
- build a shiny app to interactively explore and analyze the financial statements data using the package functions
- ensure the package is efficient and can handle large datasets typical of SEC filings
- write unit tests to ensure the correctness and reliability of the package functions
- focus on the functions to choose underpriced stocks based on the financial statements data

## Development Plan

### Phase 1: Data Processing and Ingestion
1. **Enhance SEC data processing**
   - write functions to download data from SEC website, and the zip file url is like https://www.sec.gov/files/dera/data/financial-statement-data-sets/2009q1.zip, for 2009 Q1 data.
   - Implement functions to read and parse SEC structured financial statement datasets provided by SEC, like the data in the SEC folder. This has higher priority.
   - Alternatively, we can develop tools to process xbrl data, like submissions.zip, companyfacts.zip; but this has lower priority.
   - Create functions to extract financial data from different forms (10-K, 10-Q, etc.)
   - Develop data cleaning and validation routines
   - Implement data merging across dates and forms for time series creation

2. **Data structure design**
   - Define standardized data structures for financial statements
   - Create functions to convert SEC raw data to standardized format
   - Ensure efficient handling of large datasets
   - The standardized data can be stored into a database (sqlite3) or R data frames for easy access and manipulation

### Phase 2: Financial Analysis Functions
3. **Expand financial ratio calculations**
   - Add functions to calculate key financial ratios (liquidity, profitability, efficiency, leverage)
   - Create functions for trend analysis of financial metrics
   - Enhance existing DCF model with additional valuation methods

4. **Implement stock screening functions**
   - Create functions to identify undervalued/overvalued stocks
   - Implement factor-based screening (value, growth, quality metrics)
   - Develop functions to rank stocks based on various criteria

### Phase 3: Visualization and Export
5. **Visualization functions**
   - Create functions to visualize financial statement trends
   - Implement comparison functions for peer analysis
   - Add charting capabilities for financial metrics

6. **Export functionality**
   - Implement functions to export data and analysis results
   - Support multiple formats (CSV, Excel, PDF)
   - Enable export of reports with visualizations

### Phase 4: Shiny Application
7. **Build interactive Shiny app**
   - Create dashboard for financial statement exploration
   - Implement stock screening and analysis tools
   - Add visualization and reporting features

### Phase 5: Testing and Documentation
8. **Unit tests**
   - Write comprehensive unit tests for all functions
   - Implement testing for data processing and analysis functions
   - Ensure robust error handling

9. **Documentation and examples**
   - Create comprehensive package documentation
   - Provide detailed examples for all functions
   - Include tutorials for common use cases

### Phase 6: Performance Optimization
10. **Optimize for large datasets**
    - Implement efficient data processing techniques
    - Optimize memory usage for large financial datasets
    - Consider parallel processing for computationally intensive tasks

### Phase 7: Updates and Maintenance
11. **Data update mechanisms**
    - Automate SEC data downloading and processing
    - Implement incremental updates to minimize processing time
    - Create data integrity checks

12. **Focus on underpriced stock identification**
    - Enhance valuation models for better intrinsic value estimation
    - Implement additional metrics for value investing
    - Create backtesting tools to validate strategies

### Phase 8: Implementation Status
The following elements have been implemented as part of this development plan:

**Phase 1: Data Processing and Ingestion**
- ✅ Functions to read SEC financial statement datasets (sub, num, tag, pre)
- ✅ Data cleaning and validation routines
- ✅ Data merging across dates and forms for time series creation
- ✅ Performance optimization for large files with chunked reading

**Phase 2: Financial Analysis Functions**
- ✅ Functions to calculate key financial ratios (liquidity, profitability, efficiency, leverage)
- ✅ Functions for trend analysis of financial metrics
- ✅ Enhanced DCF model with additional valuation methods
- ✅ Functions to identify undervalued/overvalued stocks
- ✅ Factor-based screening (value, growth, quality metrics)
- ✅ Functions to rank stocks based on various criteria

**Phase 3: Visualization and Export**
- ✅ Functions to visualize financial statement trends
- ✅ Functions for comparison of companies
- ✅ Charting capabilities for financial metrics
- ✅ Functions to export data and analysis results
- ✅ Support for multiple formats (CSV, Excel, PDF)

**Phase 4: Shiny Application**
- ✅ Dashboard for financial statement exploration
- ✅ Stock screening and analysis tools
- ✅ Visualization and reporting features

**Phase 5: Testing and Documentation**
- Functions are documented with roxygen comments
- Will require additional unit tests (not fully implemented in this iteration)

**Phase 6: Performance Optimization**
- Implemented chunked reading for large files
- Memory-efficient processing of large datasets

**Phase 7: Updates and Maintenance**
- Enhanced valuation models with comprehensive scoring
- Additional metrics for value investing
- Tools for identifying underpriced stocks

The existing codebase already contains some foundational elements like `update_edgar_data`, growth rate calculations, DCF models, and functions to find undervalued stocks. The development plan has enhanced these foundations to achieve all stated goals.
