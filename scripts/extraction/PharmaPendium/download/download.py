from dotenv import load_dotenv
import os
from progressbar import progressbar
from pypdf import PdfMerger
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
from selenium.webdriver.common.actions.wheel_input import ScrollOrigin
import pandas as pd
from selenium.webdriver import ActionChains

import warnings

# Environment variables
load_dotenv(".env")

# Load environmental variables
email = os.getenv("email")
password = os.getenv("password")
in_dir = os.getenv("in_dir")
out_dir = os.getenv("out_dir")

# Base url
login_url = "https://www.pharmapendium.com/welcome"

# Number of rows to skip before reaching the column_names row
skiprows = 0


def wait_for_element(driver, xpath, delay=10):
    """
    Wait for html element to be present on the page with a specified timeout.

    Args:
        driver (selenium.webdriver.chrome.webdriver.WebDriver): Selenium driver object.
        xpath (str): Html path to desired element.
        delay (int, optional): maximum wait time. Defaults to 10.

    Returns:
        selenium.webdriver.WebElement: Selenium web element.
    """
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


def download_wait(filepath, timeout=240):
    """
    Wait for downloads to finish with a specified timeout.

    Args:
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
    """
    Get the Seleneium driver object with specified settings.

    Returns:
        selenium.webdriver.chrome.webdriver.WebDriver: Selenium WebDriver object
    """
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

    # driver = webdriver.Chrome(service=service, options=Options)
    driver = webdriver.Chrome(service=service, options=Options)

    return driver


def login_to_pharmapendium(driver, url):
    """
    Login to pharmapendium website using Selenium

    Args:
        driver (selenium.webdriver.chrome.webdriver.WebDriver): Selenium Webdriver object.
        url (str): Base URL to access Pharmapendium.
    """
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


def download_pdfs_pharma(driver, urls, out_dir, merge_subsets=False):
    """
    Download a list of pharmapendium PDFs from a URL dictionary using Selenium.

    Args:
        driver (selenium.webdriver.chrome.webdriver.WebDriver): Selenium WebDriver object
        urls (dict): A dictionary of PharmaPendium URL's initialized with empty values.
        out_dir (url): The path to the directory for downloaded PDFs.
        chemical_name (str, optional): Chemical Name to append to associated PDFs. Defaults to None.
        merge_subsets (bool, optional): Whether or not to merge full documents together when subset documents are found.

    Returns:
        dict: A dictionary with the original URLs, and updated values representing the downloaded filenames.
    """

    for url in progressbar(urls):
        file_hash = None

        try:
            driver.get(url)
        except:
            continue

        # Get the chemical name from the url
        chemical_name = url.split("/")[-2].title()

        # Extract study information from the header
        header = wait_for_element(
            driver, "/html/body/div[1]/div/section/div[1]/div[3]/div/div/div/h1"
        )
        split_header = header.text.split(" ")
        doc_type = split_header[0] + " " + split_header[1]
        study_number = split_header[2]

        # Boolean to check if the study is a subset of a larger document
        is_subset = "Part" in header.text

        save_file_name = (
            str(chemical_name + "_" + doc_type + ".pdf")
            .replace(r"%20", "_")
            .replace(" ", "_")
            .replace(r"/", "_")
        )

        # Download PharmaPendium PDFs
        if "pharmapendium" in url:
            if merge_subsets and is_subset:
                try:
                    doc_button = wait_for_element(
                        driver, f"//button[@title='{doc_type}']", delay=10
                    )
                    parent = wait_for_element(doc_button, "../../..")
                    document_container = parent.find_element(By.TAG_NAME, "ul")
                    document_list = document_container.find_elements(By.TAG_NAME, "li")

                    ActionChains(driver).scroll_from_origin(
                        ScrollOrigin.from_element(document_container, 0, -50), 0, -2000
                    ).perform()

                    file_list = []
                    for document in document_list:
                        if ("Part" in document.text) and (
                            study_number in document.text
                        ):
                            print(
                                f"Downloading new document {document.text} {study_number =}"
                            )

                            try:
                                document.click()
                            except:
                                ActionChains(driver).scroll_from_origin(
                                    ScrollOrigin.from_element(
                                        document_container, 0, -50
                                    ),
                                    0,
                                    200,
                                ).perform()
                                document.click()

                            iframe = wait_for_element(
                                driver, "//iframe[@title='PDF Viewer']"
                            )
                            driver.switch_to.frame(iframe)

                            tools_button = wait_for_element(
                                driver, '//button[@title="Tools"]'
                            )
                            if tools_button:
                                tools_button.click()

                            download_button = wait_for_element(
                                driver, '//*[@id="download"]'
                            )
                            if download_button:
                                driver.execute_script(
                                    "arguments[0].click();", download_button
                                )

                            file_name = driver.current_url.split("/")[-1] + ".pdf"

                            # Wait for the file to download
                            dwl_wait = download_wait(
                                os.path.join(out_dir, file_name), 180
                            )

                            if dwl_wait is not None:
                                urls[url] = save_file_name

                            if os.path.isfile(os.path.join(out_dir, "document.pdf")):
                                os.rename(
                                    os.path.join(out_dir, "document.pdf"),
                                    os.path.join(out_dir, file_name),
                                )

                                # Wait for file to be renamed
                                while not os.path.exists(
                                    os.path.join(out_dir, file_name)
                                ):
                                    time.sleep(1)

                            file_list.append(file_name)

                            # Switch back to default window
                            driver.switch_to.default_content()

                            time.sleep(2)

                    # Merge the PDFs
                    merger = PdfMerger()
                    for pdf in file_list:
                        merger.append(os.path.join(out_dir, pdf))

                    merger.write(os.path.join(out_dir, save_file_name))
                    merger.close()

                    for pdf in file_list:
                        os.remove(os.path.join(out_dir, pdf))
                except:
                    continue
            else:
                try:
                    iframe = wait_for_element(driver, "//iframe[@title='PDF Viewer']")
                    driver.switch_to.frame(iframe)

                    tools_button = wait_for_element(driver, '//button[@title="Tools"]')
                    if tools_button:
                        tools_button.click()

                    download_button = wait_for_element(driver, '//*[@id="download"]')
                    if download_button:
                        driver.execute_script("arguments[0].click();", download_button)

                    # the filename is saved as either 'document.pdf' or 'somehash.pdf'
                    file_hash = url.split("/")[-1] + ".pdf"

                    # Wait for the file to download
                    dwl_wait = download_wait(os.path.join(out_dir, file_hash), 30)

                    # Rename the document based on whether it was saved as document.pdf or hexstring.pdf
                    if os.path.isfile(os.path.join(out_dir, "document.pdf")):
                        os.rename(
                            os.path.join(out_dir, "document.pdf"),
                            os.path.join(out_dir, save_file_name),
                        )

                        # Wait for file to be renamed
                        while not os.path.exists(os.path.join(out_dir, save_file_name)):
                            time.sleep(1)
                    elif os.path.isfile(os.path.join(out_dir, file_hash)):
                        os.rename(
                            os.path.join(out_dir, file_hash),
                            os.path.join(out_dir, save_file_name),
                        )

                        # Wait for file to be renamed
                        while not os.path.exists(os.path.join(out_dir, save_file_name)):
                            time.sleep(1)
                    else:
                        print("Some other file type encountered?")

                    if dwl_wait is not None:
                        urls[url] = save_file_name
                except:
                    continue

        # Download doi PDFs
        elif "doi" in url:
            pass

        else:
            print("Unseen url.. cannot download")

    print("Downloading pharmapendium complete!")
    return urls


def download_pdfs_doi(urls, out_dir, drug_urls=None):
    """
    Download a list of DOI PDFs from a URL dictionary using API.

    Args:
        urls (dict): A dictionary of PharmaPendium URL's initialized with empty values.
        out_dir (str): The path to the directory for downloaded PDFs.
        drug_urls (dict): Chemical names to append to associated PDFs

    Returns:
        dict: A dictionary with the original URLs, and updated values representing the downloaded filenames.
    """
    dois = [url for url in urls if "doi" in url]

    for i, doi in enumerate(dois):
        # Generate a random hex string of length 30
        file_hash = "%030x" % random.randrange(16**30) + ".pdf"
        chemical_name = drug_urls[doi]
        file_name = chemical_name + "_" + file_hash
        out = os.path.join(out_dir, file_name)
        scihub_download(doi, paper_type="doi", out=out)
        if os.path.isfile(out):
            urls[doi] = file_name

    print("Downloading dois complete!")
    return urls


def add_filenames_to_df(df, urls):
    """
    Add a collection of filenames associated to their source links to a PharmaPendium export file.

    Args:
        df (pandas.DataFrame): A PharmaPendium export Dataframe to add the file names to.
        urls (dict): A dictionary with download URLs and associated file names.

    Returns:
        pandas.DataFrame: A PharmaPendium dataframe with added file names.
    """
    df["filename"] = df["source link"].apply(
        lambda x: next((urls[url] for url in urls if url in x), None)
    )

    return df


if __name__ == "__main__":
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
            df = pd.read_excel(in_dir + filename, skiprows=skiprows, engine="openpyxl")

        # Get the unique source links
        df["source link"] = df["source link"].astype(str)
        unique_documents = {
            url.split("?", 1)[0]: None for url in df["source link"].unique()
        }
        drug_urls = {
            row["source link"]: str(drug).replace(" ", "_")
            for drug, row in df.iterrows()
        }

        # Download all unique files, if possible, and create a new excel with filename column
        urls = download_pdfs_pharma(
            driver, unique_documents, out_dir, download_full=True
        )
        df = add_filenames_to_df(df, urls)
        urls = download_pdfs_doi(urls, out_dir, drug_urls)
        df = add_filenames_to_df(df, urls)
        df.to_excel(str(out_dir + filename).replace(".xlsx", "_new.xlsx"))

        # Create file for missing pharmapendium files and doi files
        missing_pdf_names = df[df["filename"].isnull()]["source link"]
        missing_pdfs.update(missing_pdf_names)

    print("Creating missing_pdfs.txt..")
    # Create a .txt file with the missing pdfs from all the documents
    with open(os.path.join(out_dir, f"missing_pdfs.txt"), "w") as file:
        for pdf_name in missing_pdfs:
            file.write(pdf_name + "\n")

    driver.close()

    print("Process complete!")
