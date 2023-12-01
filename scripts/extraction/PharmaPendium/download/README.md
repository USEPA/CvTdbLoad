# Download.py
This script is used to download PDFs from PharmaPendium export files.

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

## Environment Variables
The script uses environment variables to store sensitive information. You need to create a .env file in the same directory as your script. The file should contain the following variables:

```
email=your_email
password=your_password
in_dir=input_directory
out_dir=output_directory
```

Replace your_email and your_password with your actual email and password. input_directory and output_directory should be the paths to your input and output directories, respectively.

## Running the Script
To run the script, use the following command:

```bash
python download.py
```

This will start the script, which will read the URLs from the input files in the input directory, download the PDFs, and save them in the output directory. It will also create a new Excel file with an additional filename column, and a text file with the links that were unable to be downloaded.