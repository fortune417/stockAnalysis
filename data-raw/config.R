## code to prepare `config` dataset goes here

#usethis::use_data(config, overwrite = TRUE)

# set URL paths for downloading SEC data
edgarData<-list(
    facts = "http://www.sec.gov/Archives/edgar/daily-index/xbrl/companyfacts.zip",
    submissions = "https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip",
    companyTickers = "https://www.sec.gov/files/company_tickers.json",
    companyTickersExchange = "https://www.sec.gov/files/company_tickers_exchange.json"
)

usethis::use_data(edgarData, overwrite=T)

# prepare data holding the tickers and company names
status<-update_edgar_data(outDir=file.path("download/edgar", today(asStr=T)))
tickerFile<-status$companyTickersExchange
tmp1<-jsonlite::read_json(tickerFile, simplifyVector = T)
tickers<-as.data.frame(tmp1$data)
names(tickers)<-tmp1$fields

usethis::use_data(tickers, overwrite=T)
