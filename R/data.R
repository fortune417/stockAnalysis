# functions to prepare data

#' SEC EDGAR data
#'
#' This object contains the URLs for the source
#' EDGAR data.
#' @format `edgarData`
#'    a list containing URLs
#' @source SEC EDGAR data API
"edgarData"

#' Update SEC EDGAR data
#'
#' This function updates the latest versions of the
#' following files from SEC database.
#' - company_tickers_exchange.json
#' - company_tickers.json
#' - submissions.zip
#' - companyfacts.zip
#' The URLs for these files are in the object [edgarData]
#'
update_edgar_data<-function(outDir=NULL) {
  stopifnot(exists("edgarData"))
  if(is.null(outDir)) {
    outDir<-today(asStr=T)
  }
  if(!dir.exists(outDir)) {
    dir.create(outDir, recursive = T)
  }
  manualDownloadFiles<-c("companyfacts.zip","submissions.zip")
  status<-sapply(names(edgarData),
         function(x) {
           url<-edgarData[[x]]
           if(basename(url) %in% manualDownloadFiles) {
             message(glue::glue("Please download '{url}' manually and put it into {outDir}"))
             return(NULL)
           }
           dest<-file.path(outDir, basename(url))
           download_file(url, destfile=dest)
           return(dest)
         })
  return(invisible(status))
}


#' Stock tickers
#'
#' This dataset contains the stock tickers listed in
#' US stock exchanges and their company names.
#'
#' @format ## `tickers`
#' A data frame with 4 columns:
#' - cik: the unique central index key for the ticker.
#' - name: company name
#' - ticker: ticker symbol
#' - exchange: listing stock exchange
#'
#' @source see [edgarData$companyTickersExchange]
"tickers"

#' Read SEC submission data (SUB)
#'
#' This function reads the submission data from SEC financial statement data sets.
#' The SUB dataset contains summary information about an entire EDGAR submission.
#'
#' @param file Path to the sub.txt file from SEC data set
#' @param n_max Number of rows to read (default: all)
#' @return A data frame containing submission data
read_sec_sub<-function(file, n_max=-1) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # Read the tab-delimited file
  dat <- utils::read.table(file,
                    header=TRUE,
                    sep="\t",
                    quote="",
                    stringsAsFactors=FALSE,
                    fill=TRUE,
                    nrows=n_max)

  # Convert CIK to character to avoid integer overflow
  if("cik" %in% colnames(dat)) {
    dat$cik <- as.character(dat$cik)
  }

  return(dat)
}

#' Read SEC tag data (TAG)
#'
#' This function reads the tag data from SEC financial statement data sets.
#' The TAG dataset includes defining information about each numerical tag.
#'
#' @param file Path to the tag.txt file from SEC data set
#' @param n_max Number of rows to read (default: all)
#' @return A data frame containing tag data
read_sec_tag<-function(file, n_max=-1) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # Read the tab-delimited file
  dat <- utils::read.table(file,
                    header=TRUE,
                    sep="\t",
                    quote="",
                    stringsAsFactors=FALSE,
                    fill=TRUE,
                    nrows=n_max)

  return(dat)
}

#' Read SEC numbers data (NUM) with performance optimization
#'
#' This function reads the numbers data from SEC financial statement data sets.
#' The NUM dataset includes one row for each distinct amount appearing on the primary financial statements.
#' This version includes performance optimization for handling large datasets.
#'
#' @param file Path to the num.txt file from SEC data set
#' @param n_max Number of rows to read (default: all, though with a large file like this, consider using a subset)
#' @param chunk_size Size of chunks to process when reading very large files (default: 100000)
#' @param cols_to_read Character vector of column names to read (if NULL, reads all)
#' @return A data frame containing numbers data
read_sec_num<-function(file, n_max=-1, chunk_size=100000, cols_to_read=NULL) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # If the file is very large, process in chunks
  file_size <- file.info(file)$size / (1024^2)  # Size in MB
  if (file_size > 500) {  # If file is larger than 500MB
    message("Processing large file in chunks...")
    dat <- read_sec_num_chunked(file, n_max, chunk_size, cols_to_read)
  } else {
    # For smaller files, use the standard approach
    if (is.null(cols_to_read)) {
      dat <- utils::read.table(file,
                        header=TRUE,
                        sep="\t",
                        quote="",
                        stringsAsFactors=FALSE,
                        fill=TRUE,
                        nrows=n_max)
    } else {
      # Read only specified columns
      all_cols <- utils::scan(file, what=character(), nlines=1, sep="\t", quiet=TRUE)
      col_classes <- rep("character", length(all_cols))
      names(col_classes) <- all_cols
      col_classes[cols_to_read] <- "character"  # Read all as character for simplicity

      dat <- utils::read.table(file,
                        header=TRUE,
                        sep="\t",
                        quote="",
                        colClasses=col_classes,
                        fill=TRUE,
                        nrows=n_max)
      # Keep only requested columns
      dat <- dat[, intersect(cols_to_read, colnames(dat)), drop=FALSE]
    }
  }

  # Convert CIK to character to avoid integer overflow
  if("cik" %in% colnames(dat)) {
    dat$cik <- as.character(dat$cik)
  }

  return(dat)
}

#' Read SEC numbers data in chunks (helper function)
#'
#' This function reads large files in chunks to manage memory usage.
read_sec_num_chunked <- function(file, n_max, chunk_size, cols_to_read) {
  # First, read header to get column names
  header <- utils::scan(file, what=character(), nlines=1, sep="\t", quiet=TRUE)

  # Set up column classes (all as character to start)
  if (is.null(cols_to_read)) {
    cols_to_read <- header
  }

  col_classes <- rep("character", length(header))
  names(col_classes) <- header
  col_classes[cols_to_read] <- "character"

  # Read in chunks
  all_chunks <- list()
  total_read <- 0
  skip_lines <- 1  # Skip header

  while (TRUE) {
    if (n_max > 0) {
      remaining <- n_max - total_read
      if (remaining <= 0) break
      current_chunk_size <- min(chunk_size, remaining)
    } else {
      current_chunk_size <- chunk_size
    }

    chunk <- utils::read.table(file,
                        sep="\t",
                        quote="",
                        colClasses=col_classes,
                        fill=TRUE,
                        skip=skip_lines,
                        nrows=current_chunk_size,
                        header=FALSE)

    # Add column names
    colnames(chunk) <- header

    # Keep only requested columns
    chunk <- chunk[, intersect(cols_to_read, colnames(chunk)), drop=FALSE]

    if (nrow(chunk) == 0) break  # No more data to read

    all_chunks[[length(all_chunks) + 1]] <- chunk
    total_read <- total_read + nrow(chunk)
    skip_lines <- skip_lines + nrow(chunk)

    if (nrow(chunk) < current_chunk_size) break  # Reached end of file
  }

  # Combine all chunks
  if (length(all_chunks) > 0) {
    dat <- do.call(rbind, all_chunks)
  } else {
    # Create empty data frame with proper structure
    dat <- data.frame(matrix(ncol=length(cols_to_read), nrow=0))
    colnames(dat) <- cols_to_read
  }

  return(dat)
}

#' Read SEC presentation data (PRE)
#'
#' This function reads the presentation data from SEC financial statement data sets.
#' The PRE dataset provides information about how the tags and numbers were presented in financial statements.
#'
#' @param file Path to the pre.txt file from SEC data set
#' @param n_max Number of rows to read (default: all)
#' @return A data frame containing presentation data
read_sec_pre<-function(file, n_max=-1) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }

  # Read the tab-delimited file
  dat <- utils::read.table(file,
                    header=TRUE,
                    sep="\t",
                    quote="",
                    stringsAsFactors=FALSE,
                    fill=TRUE,
                    nrows=n_max)

  return(dat)
}

#' Process SEC financial statement data sets
#'
#' This function processes SEC financial statement data sets (sub, num, tag, pre)
#' and creates a unified data structure for financial analysis.
#'
#' @param data_dir Directory containing SEC data files (sub.txt, num.txt, tag.txt, pre.txt)
#' @param ticker Optional ticker to filter data for a specific company
#' @param cik Optional CIK to filter data for a specific company
#' @param years Optional vector of years to filter data
#' @return A list containing processed financial data
process_sec_data<-function(data_dir, ticker=NULL, cik=NULL, years=NULL) {
  # Define file paths
  sub_file <- file.path(data_dir, "sub.txt")
  num_file <- file.path(data_dir, "num.txt")
  tag_file <- file.path(data_dir, "tag.txt")
  pre_file <- file.path(data_dir, "pre.txt")

  # Check if files exist
  files <- c(sub_file, num_file, tag_file, pre_file)
  missing_files <- files[!file.exists(files)]
  if(length(missing_files) > 0) {
    stop("Missing files: ", paste(missing_files, collapse=", "))
  }

  # Read the data files
  message("Reading SEC submission data...")
  sub_data <- read_sec_sub(sub_file)

  message("Reading SEC tag data...")
  tag_data <- read_sec_tag(tag_file)

  # Read a sample of num data if the full file is too large
  # The num file is very large (gigabytes), so we might need to sample or process in chunks
  message("Reading SEC numbers data...")
  num_data <- read_sec_num(num_file, n_max=100000)  # Read first 100K rows as a sample

  # If specific company requested, filter by CIK
  if(!is.null(cik)) {
    sub_data <- sub_data[sub_data$cik == as.character(cik), ]
    if(nrow(sub_data) == 0) {
      stop("No data found for CIK: ", cik)
    }
    # Filter num_data to only include relevant adsh values
    adsh_values <- sub_data$adsh
    num_data <- num_data[num_data$adsh %in% adsh_values, ]
  }

  # If ticker is provided, try to find the CIK first
  if(!is.null(ticker) && is.null(cik)) {
    # We'd need a lookup table to map ticker to CIK
    # For now, we'll show an approach
    # In a real implementation, we'd need the ticker to CIK mapping
    message("Note: Ticker to CIK mapping required for filtering by ticker")
  }

  # If years are specified, filter the data
  if(!is.null(years)) {
    if("period" %in% colnames(sub_data)) {
      # Extract year from period (assuming format YYYYMMDD)
      sub_data$year <- as.numeric(substr(sub_data$period, 1, 4))
      sub_data <- sub_data[sub_data$year %in% years, ]
      # Filter num_data accordingly
      adsh_values <- sub_data$adsh
      num_data <- num_data[num_data$adsh %in% adsh_values, ]
    }
  }

  # Create a unified data structure
  sec_data <- list(
    submissions = sub_data,
    tags = tag_data,
    numbers = num_data,
    meta = list(
      data_dir = data_dir,
      filter = list(
        ticker = ticker,
        cik = cik,
        years = years
      ),
      processed_date = Sys.time()
    )
  )

  return(sec_data)
}

#' Merge SEC data across dates and forms
#'
#' This function merges SEC financial data across different dates and forms
#' to create a time series of financial statements for each company.
#'
#' @param sec_data A list containing processed SEC data (from process_sec_data)
#' @param include_qtr Logical, whether to include quarterly (10-Q) data
#' @param include_annual Logical, whether to include annual (10-K) data
#' @return A data frame with merged financial data
merge_sec_data<-function(sec_data, include_qtr=TRUE, include_annual=TRUE) {
  if (!is.list(sec_data) || !all(c("submissions", "tags", "numbers") %in% names(sec_data))) {
    stop("sec_data must be a list with 'submissions', 'tags', and 'numbers' components")
  }

  sub_data <- sec_data$submissions
  num_data <- sec_data$numbers
  tag_data <- sec_data$tags

  # Filter forms if needed
  if (!include_qtr && !include_annual) {
    stop("At least one of include_qtr or include_annual must be TRUE")
  }

  form_filter <- c()
  if (include_qtr) form_filter <- c(form_filter, "10-Q")
  if (include_annual) form_filter <- c(form_filter, "10-K")

  if (length(form_filter) > 0 && "form" %in% colnames(sub_data)) {
    sub_data <- sub_data[sub_data$form %in% form_filter, ]
    # Filter num_data accordingly
    num_data <- num_data[num_data$adsh %in% sub_data$adsh, ]
  }

  # Merge number data with submission data using adsh
  merged_data <- merge(num_data, sub_data[, c("adsh", "cik", "name", "period", "form", "fy")],
                       by="adsh", all.x=TRUE)

  # Convert period to date
  if("period" %in% colnames(merged_data)) {
    merged_data$period_date <- as.Date(as.character(merged_data$period), format="%Y%m%d")
  }

  # Add year column
  if("period_date" %in% colnames(merged_data)) {
    merged_data$year <- as.numeric(format(merged_data$period_date, "%Y"))
  }

  # Include tag descriptions
  if("tag" %in% colnames(merged_data) && "version" %in% colnames(merged_data)) {
    # Merge with tag data to get tag descriptions
    merged_data <- merge(merged_data,
                         tag_data[, c("tag", "version", "tlabel", "datatype")],
                         by=c("tag", "version"), all.x=TRUE)
  }

  # Order by CIK, period, and tag for time series
  merged_data <- merged_data[order(merged_data$cik, merged_data$period_date, merged_data$tag), ]

  return(merged_data)
}

