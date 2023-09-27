from dotenv import load_dotenv
import os
from progressbar import progressbar
import random
from scidownl import scihub_download
import time

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote import errorhandler
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

import pandas as pd

import warnings

# Environment variables
load_dotenv(".env")

email = os.getenv("email")
password = os.getenv("password")
in_dir = os.getenv("in_dir")
out_dir = os.getenv("out_dir")

login_url = "https://www.pharmapendium.com/welcome"

# Number of rows to skip before reaching the column_names row
skiprows = 5


def wait_for_element(driver, xpath, delay=10):
    try:
        myElem = WebDriverWait(driver, delay).until(
            EC.presence_of_element_located(
                (
                    By.XPATH,
                    xpath,
                )
            )
        )
    except errorhandler.TimeoutException:
        print("Loading took too much time!")
        return None

    return myElem


def download_wait(filepath, timeout=120):
    """
    Wait for downloads to finish with a specified timeout.

    Args
    ----
    filepath : str
        The path to the file to wait for.
    timeout : int
        How many seconds to wait until timing out.

    """
    seconds = 0
    dl_wait = True
    while dl_wait and seconds < timeout:
        time.sleep(1)
        if os.path.isfile(filepath) or os.path.isfile(out_dir + "document.pdf"):
            dl_wait = False
        seconds += 1
    if seconds == timeout:
        # print("Download exceeded timeout limit..")
        return None
    else:
        return seconds


def get_driver():
    print("Setting up web driver...")
    Options = webdriver.ChromeOptions()
    prefs = {
        "download.prompt_for_download": False,
        "download.directory_upgrade": True,
        "download.default_directory": out_dir,
        "profile.default_content_settings.popups": False,
        "profile.default_content_setting_values.automatic_downloads": True,
        "safebrowsing.enabled": True,
    }
    Options.add_experimental_option("prefs", prefs)
    Options.add_experimental_option("excludeSwitches", ["enable-logging"])
    Options.add_argument("--start-fullscreen")
    Options.add_argument("--headless")
    service = Service()

    driver = webdriver.Chrome(service=service, options=Options)

    return driver


def login_to_pharmapendium(driver, url):
    # Navigate to the base URL
    print("Navigating to base url...")
    driver.get(url)
    print("Logging in...")
    login = wait_for_element(
        driver,
        '//button[@class="Button-module_root__8RX49 Button-module_primary__st6yY"]',
    )
    if login:
        login.click()

    email_field = wait_for_element(driver, '//input[@id="bdd-email"]')
    # Enter login email
    email_field = driver.find_element(
        By.XPATH,
        '//input[@id="bdd-email"]',
    )
    if email_field:
        email_field.send_keys(email)
        email_field.send_keys(Keys.ENTER)
    password_field = wait_for_element(driver, '//input[@id="bdd-password"]')

    if password_field:
        password_field.send_keys(password)
        password_field.send_keys(Keys.ENTER)

    # Wait for login to complete
    while driver.current_url != "https://www.pharmapendium.com/home":
        print("Waiting for login to complete...")
        driver.implicitly_wait(1)

    # Accept Cookies Banner
    acc_cookies = wait_for_element(
        driver, '//button[ contains(text(), "Accept all cookies")]'
    )
    if acc_cookies:
        acc_cookies.click()


def download_pdfs_pharma(driver, urls, out_dir, chemical_name=None):
    for url in progressbar(urls):
        file_hash = None

        try:
            driver.get(url)
        except:
            continue

        # Download PharmaPendium PDFs
        if "pharmapendium" in url:
            try:
                iframe = wait_for_element(driver, "//iframe[@title='PDF Viewer']")
                driver.switch_to.frame(iframe)

                tools_button = wait_for_element(driver, '//button[@title="Tools"]')
                if tools_button:
                    tools_button.click()

                # download_button = wait_for_element(driver, '//button[@title="Save"]')
                download_button = wait_for_element(driver, '//*[@id="download"]')
                if download_button:
                    driver.execute_script("arguments[0].click();", download_button)

                # the filename is saved as either 'document.pdf' or 'somehash.pdf'
                file_hash = url.split("/")[-1] + ".pdf"
                file_name = chemical_name + "_" + file_hash

                # Wait for the file to download
                dwl_wait = download_wait(os.path.join(out_dir, file_hash), 10)

                # Rename the document based on whether it was saved as document.pdf or hexstring.pdf
                if os.path.isfile(os.path.join(out_dir, "document.pdf")):
                    os.rename(
                        os.path.join(out_dir, "document.pdf"),
                        os.path.join(out_dir, file_name),
                    )

                    # Wait for file to be renamed
                    while not os.path.exists(os.path.join(out_dir, file_name)):
                        time.sleep(1)
                elif os.path.isfile(os.path.join(out_dir, file_hash)):
                    os.rename(
                        os.path.join(out_dir, file_hash),
                        os.path.join(out_dir, file_name),
                    )

                    # Wait for file to be renamed
                    while not os.path.exists(os.path.join(out_dir, file_name)):
                        time.sleep(1)

                if dwl_wait is not None:
                    urls[url] = file_name
            except:
                print(f"Error downloading pdf from {url}")
                continue

        # Download doi PDFs
        elif "doi" in url:
            pass

        else:
            print("Unseen url.. cannot download")

    print("Downloading pharmapendium complete!")
    return urls


def download_pdfs_doi(urls, out_dir, chemical_name=None):
    dois = [url for url in urls if "doi" in url]

    for i, doi in enumerate(dois):
        # Generate a random hex string of length 30
        file_hash = "%030x" % random.randrange(16**30) + ".pdf"
        file_name = chemical_name + "_" + file_hash
        print(file_name)
        out = os.path.join(out_dir, file_name)
        scihub_download(doi, paper_type="doi", out=out)
        if os.path.isfile(out):
            urls[doi] = file_name

    print("Downloading dois complete!")
    return urls


def add_filenames_to_df(df, urls):
    df["filename"] = df["Source Link"].apply(
        lambda x: next((urls[url] for url in urls if url in x), None)
    )

    return df


# Create selenium driver and login to pharmapendium for pdf scraping
driver = get_driver()
login_to_pharmapendium(driver, login_url)

# Get the list of files to loop through
files = os.listdir(in_dir)

# Initialize a set of missing pdfs
missing_pdfs = set()

# Loops through each unique document link and downloads the pdf, where applicable
# Outputs to the output directory the pdfs, a new excel sheet with filename column, and
# a text file with the links that were unable to be downloaded
for i, filename in enumerate(files):
    print(f"Attemping to download PDFs from {filename}")
    # Read in the excel file
    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        df = pd.read_excel(
            in_dir + filename, skiprows=skiprows, index_col=0, engine="openpyxl"
        )

    # Get the unique source links
    df = df[df["Concomitants"].astype(str).str.lower().isin(["fed", "fasted", "nan"])]
    df["Source Link"] = df["Source Link"].astype(str)
    unique_documents = {
        url.split("?", 1)[0]: None for url in df["Source Link"].unique()
    }
    chemical_name = df.head(1).Drug[0]

    # Download all unique files, if possible, and create a new excel with filename column
    urls = download_pdfs_pharma(driver, unique_documents, out_dir, chemical_name)
    df = add_filenames_to_df(df, urls)
    urls = download_pdfs_doi(urls, out_dir, chemical_name)
    df = add_filenames_to_df(df, urls)
    df.to_excel(str(out_dir + filename).replace(".xlsx", "_new.xlsx"))

    # Create file for missing pharmapendium files and doi files
    missing_pdf_names = df[df["filename"].isnull()]["Source Link"]
    missing_pdfs.update(missing_pdf_names)

print("Creating missing_pdfs.txt..")
# Create a .txt file with the missing pdfs from all the documents
with open(os.path.join(out_dir, f"missing_pdfs.txt"), "w") as file:
    for pdf_name in missing_pdfs:
        file.write(pdf_name + "\n")

driver.close()

print("Process complete!")
