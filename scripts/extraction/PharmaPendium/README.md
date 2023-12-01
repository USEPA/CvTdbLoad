# Extract.py
This script is used to extract CvT templates from pharmapendium documents. This file is typically to be run after running the download script, as it requires the filename column to be present.

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
python extract.py
```

This will start the script, which will read the PharmaPendium export file from the input directory, and create CvT templates for each unique document within the file.
