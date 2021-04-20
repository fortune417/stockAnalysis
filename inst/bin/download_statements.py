#!/usr/bin/env python

import time
from datetime import date
import sys
import os
import argparse as ap
import logging
import traceback
from random import sample
from selenium import webdriver
from selenium.common.exceptions import TimeoutException, WebDriverException, NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.expected_conditions import presence_of_element_located
from selenium.webdriver.chrome.options import Options
from random_user_agent.user_agent import UserAgent
from random_user_agent.params import SoftwareName, OperatingSystem

# classes
class Request():
    '''
    A class to handle selenium request
    '''
    logger = logging.getLogger(__name__)
    # total number of tries by all instances
    retries = 0
    def __init__(self, outDir, max_tries=3):
        # web browser setting
        prefs = {"download.default_directory": outDir,
                 "download.prompt_for_download": False,
                 "download.directory_upgrade": True
                }
        self.wd_prefs = prefs
        self.max_tries = max_tries
        # tries by this instance
        self.tries = 0

    def selenium_run(self, url, open_browser=False):
        try:
            # set up the parameters for webdriver
            browser_name = [SoftwareName.CHROME.value]
            op_sys = [OperatingSystem.WINDOWS.value,
                     OperatingSystem.LINUX.value]
            ua_rotator = UserAgent(
                    software_names=browser_name,
                    operating_systems=op_sys,
                    limit=100)
            ua = ua_rotator.get_random_user_agent()

            opts = Options()
            opts.add_argument("--no-sandbox")
            opts.add_argument("--window-size=1420,1080")
            opts.add_argument("--disable-gpu")
            opts.add_argument(f"--user-agent={ua}")
            if not open_browser:
                opts.add_argument("--headless")
            opts.add_experimental_option('prefs', self.wd_prefs)
            
            # start the webdriver to work
            browser = webdriver.Chrome(options=opts)
            wait = WebDriverWait(browser, 600)
            browser.get(url)
            self.rest()
            browser.maximize_window()
            try:
                browser.find_element(By.ID, "expxlxs").click()
            except:
                logger.error(f"expxlxs button not found on {url}")
                return(None)
            self.rest()
            browser.close()
            return("Success")
        except (TimeoutException, WebDriverException) as e:
            self.logger.error(traceback.format_exc())
            if self.tries >= self.max_tries:
                self.logger.warning(
                        "[Failed] %s" % (url) )
                return(None)
            # otherwise have another try
            self.rest()
            self.retries += 1
            self.tries += 1
            self.logger.info("Selenium retry #: " + str(self.retries))
            # try one more time to download
            return(self.selenium_run(url))

    def rest(self, t=None):
        '''
        sleep for some time
        '''
        t = sample(range(5,30),1)[0] if t is None else t
        time.sleep(t)

# functions

def download_financial_statements(stock, out_dir, annual=True, quarter=True,
        *args, **kwargs):
    """
    Download the financial statements for the stock
    """
    stock=stock.lower()
    # example URL
    # https://stockanalysis.com/stocks/unh/financials/?period=quarterly
    base_url = f'https://stockanalysis.com/stocks/{stock}/financials/'
    names = ["Income", "Balance sheet", "Cash flow", "Ratio"]
    # Annual statements for income, bs, cf, and ratio
    period = 'annual'
    suffixes = ["", "balance-sheet/", "cash-flow-statement/",
            "ratios/"]
    annual_urls = [ base_url + sf for sf in suffixes ]
    quarter_urls = [ x + '?period=quarterly' for x in annual_urls ]
    def __download(urls, period):
        #nonlocal out_dir
        #nonlocal names
        # initialize the class
        req = Request(outDir=out_dir)
        # download each URL now
        for i in range(len(urls)):
            url = urls[i]
            name = names[i]
            # None if failed, page source if succeed
            status = req.selenium_run(url, *args, **kwargs)
            if status is None:
                logger.info(f"{stock}: {name}/{period} failed")
            else:
                logger.info(f"{stock}: {name}/{period} succeeded")
    if annual:
        __download(annual_urls, 'annual')

    if quarter:
        __download(quarter_urls, 'quarter')

    logger.info(f"{stock} statement downloads done")


def _read_stock_file(f, sep="\t"):
    '''
    Read stock symbols from a file
    '''
    res=[]
    if not f:
        return res
    with open(f, "r") as fh:
        for row in fh:
            stock = row.rstrip().split(sep)[0].strip()
            if stock:
                res.append(stock)
    return(res)

## Main function
def main():
    '''
    Print the usage information
    '''
    # global variables
    day=date.today()
    defaultOutDir=day.strftime("%Y-%m-%d")
    # argument setting
    desc="""
    This program downloads financial statements from the website
    'stockanalysis.com' for given stocks.

    Options (default values in []):
    """

    epilog="""
    Example uses:

    %prog -s unh bmy

    Author: Zhenguo Zhang
    Email: zhangz.sci@gmail.com
    """

    op = ap.ArgumentParser(
            description=desc,
            epilog=epilog,
            formatter_class=ap.RawTextHelpFormatter
            )

    op.add_argument(
            "-s",
            "--stocks",
            help="""A list of stock symbols for which statements
            will be downloaded""",
            dest="stocks",
            action="store",
            nargs="*",
            required=False
            )

    op.add_argument(
            "-f",
            "--file",
            help="A text file containing stock symbols, one per line",
            action="store",
            dest="stockFile",
            metavar="stock-file"
            )

    op.add_argument(
            "--no-annual",
            help="Logical option. If provided, skip annual statements",
            action="store_true",
            dest="noAnnual"
            )

    op.add_argument(
            "--no-quarter",
            help="Logical option. If provided, skip quarterly statements",
            action="store_true",
            dest="noQuarter"
            )

    op.add_argument(
            "-o",
            "--outdir",
            help="Output directory [%(default)s]",
            dest="outDir",
            default=defaultOutDir
            )

    op.add_argument(
            "--debug",
            help="If provided, more debug info will be output",
            dest="debug",
            action="store_true"
            )

    args = op.parse_args()
    # set logger
    logLevel = logging.DEBUG if args.debug else logging.INFO
    logFile=os.path.join('logs', "download_fs_" +
            day.strftime("%Y%m%d")  + ".log")
    os.makedirs("logs", exist_ok=True)
    print(f"Running information is written to {logFile}")
    logging.basicConfig(
            filename=logFile,
            filemode="w",
            format="[%(asctime)s %(levelname)s] %(name)s - %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
            level=logLevel
            )
    global logger
    logger = logging.getLogger(__name__)
    logger.info("Cmd run: %s" % (" ".join(sys.argv)) )
    # check arguments
    if args.stockFile is None and args.stocks is None:
        logger.error("""At least one of the following options are
                required: --stocks, --file""")
        sys.exit(1)
    out_dir = os.path.abspath(args.outDir)
    os.makedirs(out_dir, exist_ok=True)
    with_annual  = False if args.noAnnual else True
    with_quarter = False if args.noQuarter else True
    # get all stocks symbols
    stocks = args.stocks if args.stocks else []
    stocks.extend(_read_stock_file(args.stockFile))
    stocks = set(stocks)
    logger.info("%d stocks will be downloaded" % (len(stocks),) )
    # start downloading
    for stock in stocks:
        download_financial_statements(
                stock, 
                out_dir, 
                #open_browser=True, # open browser for debug
                quarter=with_quarter,
                annual=with_annual)
    logger.info("Job is done")

logger=None
if __name__ == "__main__":
    main()


