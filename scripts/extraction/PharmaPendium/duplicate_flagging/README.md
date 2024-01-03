# Duplicate_flagging.py
This script is used to flag duplicates from a PharmaPendium export excel file, and add a new column named 'duplicate', populated with either 'true' or 'false'. Documents with true represent duplicated children entries, whereas documents with false represent either parent documents to duplicated rows, or non-duplicated rows. This script will create a new excel file with the original file_name appended with '_revised' before '.xlsx'.

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

## Update file_path
Next, update the file_path in the file named 'duplicate_flagging.py' to the path of the PharmaPendium excel file.


## Running the Script
To run the script, use the following command:

```bash
python duplicate_flagging.py
```
