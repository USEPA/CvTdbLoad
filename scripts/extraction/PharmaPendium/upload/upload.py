from dotenv import load_dotenv
import pandas as pd
import os
import warnings
from pyclowderext.pyclowderext import upload_file_to_clowder

# Get environment variables
load_dotenv(".env")

file_path = os.getenv("filepath")
pdf_path = os.getenv("pdfpath")
dest_path = os.getenv("destpath")

apiKey=os.getenv("clowderAPI")
baseurl=os.getenv("baseurl")
dataset_id = os.getenv("datasetID")
folder_id = os.getenv("folderID")

def upload_file_to_clowder(files):
    """
    Upload files from input directory and log files to Clowder.
    Update log files to include new Clowder ID for corresponding file

    Args:
        files (list): List of log files to process.
    """
    for doc in files:
        print(f"Attempting to upload files from {doc} to clowder..")
        try:
            doc_path = os.path.join(file_path, doc)
            with warnings.catch_warnings(record=True):
                warnings.simplefilter("always")
                df = pd.read_excel(doc_path, engine="openpyxl", index_col=0)
            
            unique_documents = df[df.filename.notnull()]["filename"].unique()
        except:
            print(f"Error reading file {doc}")
            break
        
        for document in unique_documents:
            pdf_file = os.path.join(pdf_path, document)
        
            clowder_df = upload_file_to_clowder([pdf_file], dataset_id, baseurl, apiKey, folder_id)

            # Get the clowder id from the recently uploaded file
            clowder_id = clowder_df['id'].values[0]

            # Add the clowder_id to the column "clowder_id" in the dataframe
            df.loc[df['filename'] == document, 'clowder_id'] = clowder_id
        
        df.to_excel(os.path.join(dest_path, doc))
        print(f"Upload from {doc} to clowder complete!\n")

    print("Clowder uploading complete!")

if __name__=="__main__":
    # Pull files from input file_path directory
    files = os.listdir(file_path)
    # Run upload
    upload_file_to_clowder(files)