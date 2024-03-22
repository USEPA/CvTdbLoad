# Duplicate_flagging.py
This script is used to compare original Pharmapendium files to their respective QC'd versions in order to obtain statistical results on datapoints such as row counts, new columns added, the most altered columns, etc.

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

## Update input_path
Next, update the input_path in the file named 'compare.py' to the path of all original and revised excel files, the default is a folder named "/inputs/" in the compare folder. The original files must be in the form "Chemical_Pharmacokinetic_Concentrations.xlsx", and the revised files must be in the form "Chemical_CvT_QC_curatorid.xlsx". With both files present for each of the chemicals, the file matching will be performed automatically.


## Running the Script
To run the script, use the following command:

```bash
python compare.py
```