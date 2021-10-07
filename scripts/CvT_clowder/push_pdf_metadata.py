# -*- coding: utf-8 -*-
"""
Created on Fri Aug 13 12:19:53 2021

@author: JWALL01
"""

import os
#pip install entrezpy
import entrezpy.esummary.esummarizer
import pandas as pd
import shutil
import pyclowderext.pyclowderext as pc_ext
import datetime

def get_pubmed_metadata(pmid, email):
    #Check inputs
    if isinstance(pmid, int) or isinstance(pmid, str):
        pmid = [pmid]
    elif not isinstance(pmid, list):
        print("Must pass either a single PMID or list of PMID's")
        return None
    #Set-up query summarizer
    e = entrezpy.esummary.esummarizer.Esummarizer("entrezpy",
                                                  email,
                                                  apikey=None,
                                                  apikey_var=None,
                                                  threads=None,
                                                  qid=None)
    #Query
    analyzer = e.inquire({'db' : 'pubmed', 'id' : pmid})#[11850928, 11482001]})
    #Get query results
    query = analyzer.get_result().summaries
    
    return query
    
def prep_pdf_metadata(dsID, spaceID, baseurl, apiKey, email):
    #Get files in CvT Data and create metadata
    dsID = pc_ext.get_clowder_dataset_id(uuid=dsID,#"CvT PDFs",
                                         spaceID=spaceID,
                                         baseurl=baseurl,
                                         apiKey=apiKey)
    cl_files = pc_ext.get_files_by_dataset(dsID, baseurl=baseurl, apiKey=apiKey, recursive=True)
    #Filter to PMID files
    cl_files = cl_files[cl_files['filename'].str.startswith("PMID")]
    #Get PMID values from PDF filenames
    cl_files["PMID"] = cl_files.filename.apply(lambda x: int(os.path.splitext(x)[0].replace('PMID', '')))
    #Get PubMed Metadata
    query = get_pubmed_metadata(pmid=cl_files.PMID.tolist(), email=email)
    
    #Extract entries of interest
    out = {}
    for p in query.keys():
        i_keys = ['title', 'sortfirstauthor', 'fulljournalname', 'pubdate']
        if all(k in query[p].keys() for k in i_keys):
            out[p] = { your_key: query[p][your_key] for your_key in i_keys }
        else:
            print(f"Problem extracting: {p}")
    #Format as dataframe    
    pubmed = pd.DataFrame.from_dict(out).transpose()
    pubmed.index.name = "PMID"
    pubmed = pubmed.reset_index()
    pubmed = pubmed.rename(columns={"title": "Article Title", "sortfirstauthor": "First Author",
                                    "fulljournalname": "Journal", "pubdate": "Publication Year"})
    
    #Combine and fix metadata for pushing
    metadata = cl_files.merge(pubmed)
    #Select Columns of Interest
    out = metadata[['id', 'PMID', 'Article Title', 'First Author', 'Journal', 'Publication Year']]
    return out

def move_endnote_pdfs(from_path):
    """
    Parameters
    ----------
    from_path : str, required
        A file directory path to EndNote .Data folder with a PDF folder. Moves all
        nested PDF files into a new "CvT PDFs" folder in the .Data folder.

    Returns
    -------
    None. Files are copied to a "CvT PDFs" directory

    """
    #https://cyluun.github.io/blog/manipulating-python-oswalk
    f_list = []
    for r, dirs, files in os.walk(from_path + "/PDF"):
        for file in files: #Join to get full path
            if not file.startswith("."):
                f_list.append(os.path.normpath(os.path.join(r,file)))
    
    new_path = rf'{from_path}/CvT PDFs'
    if not os.path.isdir(new_path):
        os.mkdir(new_path)
    for f in f_list:
        try:
            int(os.path.splitext(os.path.basename(f))[0])
            new_name = rf'{new_path}/PMID{os.path.basename(f)}'
        except:
            new_name = rf'{new_path}/{os.path.basename(f)}'
        if not os.path.isfile(new_name):
            shutil.copy(f, new_name)

def upload_endnote_pdfs(endnote_dir, spaceID, baseurl, apiKey):
    #endnote_dir = r"C:\Users\JWALL01\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\CvT Endnote\PMID Project 2020"
    #Get EndNote .Data Directories
    fd_list = [f for f in os.listdir(endnote_dir) if ".Data" in f]
    #Copy EndNote PDFs to CvT PDFs and rename within .Data folders
    for fd in fd_list:
        move_endnote_pdfs(from_path = rf"{endnote_dir}/{fd}")
    #Upload to Clowder
    #Can only do Found_PDFs_Stage1.Data, ILLs_Phase3_Batch1.Data, ILLs_Phase3_Batch2.Data, ILLs_Phase3_Batch3.Data, ILLs_Phase3_Batch4.Data
    for fd in fd_list:
        if fd in ["Found_PDFs_Stage1.Data", 
                  "ILLs_Phase3_Batch1.Data", "ILLs_Phase3_Batch2.Data", 
                  "ILLs_Phase3_Batch3.Data", "ILLs_Phase3_Batch4.Data"]:
            pc_ext.upload_file_directory(fileDir=f"{endnote_dir}/{fd}/CvT PDFs",
                                         description="",
                                         spaceID=spaceID, baseurl=baseurl, apiKey=apiKey)
    
if __name__ == "__main__":
    args = pc_ext.parse_cmd(cmd_type="push_metadata")
    apiKey = args.apiKey
    baseurl = args.baseurl
    spaceID = args.spaceUUID
    userID = args.userID
    
    #print("Uploading EndNote...")
    # upload_endnote_pdfs(endnote_dir = r"C:\Users\JWALL01\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\CvT Endnote\PMID Project 2020", 
    #                     spaceID=spaceID, baseurl=baseurl, apiKey=apiKey)
    #endnote_dir = r"C:\Users\JWALL01\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\CvT Endnote\PMID Project 2020"
    
    #Prep Metadata for Loaded Files
    out = prep_pdf_metadata(dsID="CvT PDFs", spaceID=spaceID, 
                            baseurl=baseurl, apiKey=apiKey, email="")
    #Push metadata
    pc_ext.upload_file_metadata(metadata=out, idCol="id", userID=userID, baseurl=baseurl, apiKey=apiKey)
    #Save copy of metadata
    out.to_csv(f"CvT_PDF_metadata_{str(datetime.datetime.now().strftime('%Y_%m_%d-%I_%M_%S'))}.csv",
               index=False)
    
    #Compare metadata to see if already pushed (one-by-one for now...so skipped for now)
    #compare_meta = [pc_ext.compare_metadata(fileID=out['id'][i], new_metadata=out.iloc[[i], out.columns != 'id'], 
    #                            baseurl=baseurl, apiKey=apiKey) for i in out.index]
    # if not all(c is None for c in compare_meta):
    #     #Filter to fileID values compare_metadata flagged to push
    #     pc_ext.upload_file_metadata(metadata=out[out['id'].isin(compare_meta)], idCol='id', 
    #                          userID=userID, baseurl=baseurl, apiKey=apiKey)
    # else:
    #     print("...no new metadata to push...returning")  
    print(f"Done... - {str(datetime.datetime.now())}")
        
        
    

# Article Title : Disposition of orally administered di-(2-ethylhexyl) phthalate and mono-(2-ethylhexyl) phthalate in the rat.
# Author Firstname : O A
# Author Lastname : Teirlynck
# collectionID : 60a551e69932c7c0b510f3da
# created_at : 2021-05-19 14:19:38
# datasetID : 60a552039932c7c0b510f3dd
# filename : PMID4091646.pdf
# filePath : L:/Lab/NCCT_ExpoCast/ExpoCast2021/CvT-CompletedTemplates/CvT PDFs/PMID4091646.pdf
# folderID :
# journal : Archives of toxicology
# PMID : 4091646
# Publication Year : 1986
