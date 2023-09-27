# Upload.py
This script is used to upload files to Clowder using the pyclowderext library. It reads PharmaPendium export files from a specified directory, uploads them to Clowder, and adds the Clowder ID to the corresponding row in an Excel file.

## Setup
Virtual Environment
First, you need to set up a virtual environment. This can be done using the following commands:

```bash
python3 -m venv env
env/bin/activate
```

## Install Requirements
Next, install the necessary Python packages using pip:

```bash
pip install -r requirements.txt
```

We also need to clone and install pyclowderext from github:
```bash
git clone https://github.com/USEPA/pyclowderext.git
cd pyclowderext
pip install .
```

## Environment Variables
The script uses environment variables to store sensitive information. You need to create a .env file in the same directory as your script. The file should contain the following variables:

```
filepath=file_directory
pdfpath=pdf_directory
destpath=destination_directory
clowderAPI=your_clowder_API_key
baseurl=your_base_url
datasetID=your_dataset_ID
folderID=your_folder_ID
```

Replace file_directory, pdf_directory, and destination_directory should be the paths to your file, PDF, and destination directories, respectively. your_clowder_API_key, your_base_url, your_dataset_ID, and your_folder_ID should be replaced with your actual Clowder API key, base URL, dataset ID, and folder ID.

## Running the Script
To run the script, use the following command:

```bash
python upload.py
```

This will start the script, which will upload Pharmapendium PDFs to Clowder, and create a new column with the clowder_ids within the export file.