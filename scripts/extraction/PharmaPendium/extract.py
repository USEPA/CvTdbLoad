from dotenv import load_dotenv
import pandas as pd
import numpy as np
import os
import random
import re
import warnings

load_dotenv(".env")

studies_group = ["Drug", "Dose", "Duration", "Route"]
subjects_group = ["Species", "Sex", "Age"]
series_group = [
    "Parameter",
    "Units",
    "Species",
    "Sex",
    "Age",
    "Dose",
    "Duration",
    "Route",
]


def extract_documents_sheet(df):
    """
    Extract the relevant PharmaPendium columns, and convert them to CvTDB template format

    Args:
        df (pandas.DataFrame): The PharmaPendium file as a DataFrame

    Returns:
        dict: The extracted CvTDB fields
    """
    documents = df.groupby(["Document"], dropna=False).head(1)
    num_documents = len(documents)

    # Set id column
    ids = [i for i in range(1, num_documents + 1)]

    # Set document_type column
    document_types = [1] * num_documents

    # Extract other_study_identifier columns
    other_study_identifiers = documents["Study Number"].tolist()

    # Extract doi column
    source_links = documents["Source Link"].astype(str).tolist()
    dois = [
        x.replace("http://dx.doi.org/", "") if "doi" in x else None
        for x in source_links
    ]

    # Extract year column
    years = documents["Year"].tolist()

    # Extract title column
    titles = documents["Study Name"].tolist()

    # Extract url column
    source_links = documents["Document"].astype(str).tolist()
    urls = [None if "doi" in x else x for x in source_links]

    # Extract curator_comment column
    curator_comments = documents["filename"].tolist()

    return {
        "id": ids,
        "document_type": document_types,
        "other_study_identifier": other_study_identifiers,
        "doi": dois,
        "year": years,
        "title": titles,
        "url": urls,
        "curator_comment": curator_comments,
    }


def extract_studies_sheet(df):
    """
    Extract the relevant PharmaPendium columns, and convert them to CvTDB template format

    Args:
        df (pandas.DataFrame): The PharmaPendium file as a DataFrame

    Returns:
        dict: The extracted CvTDB fields
    """
    studies = df.groupby(["Document"].extend(studies_group), dropna=False).head(1)
    num_studies = len(studies)

    # Extract id column
    ids = [i for i in range(1, num_studies + 1)]

    # Extract fk_reference_document column
    documents = df.groupby(["Document"]).head(1).reset_index()
    documents["id"] = documents.index + 1

    def get_id(row):
        return documents[documents["Document"] == row]["id"].values[0]

    studies = studies.assign(fk_reference_document_id=studies["Document"].apply(get_id))

    fk_reference_document_ids = studies["fk_reference_document_id"].tolist()

    # Extract test_substance_name column
    test_substance_names = studies["Drug"].tolist()

    # Extract dose_level column
    dose_levels = (
        studies["Dose"].apply(lambda x: "/".join(re.findall(r"[\d.]+", x))).tolist()
    )

    # Extract dose_level_units column
    dose_level_units = (
        studies["Dose"].apply(lambda x: "/".join(re.findall(r"[a-zA-Z]+", x))).tolist()
    )

    # Extract administration_route column
    administration_routes = studies["Route"].tolist()

    # Extract administration_term column
    administration_terms = (
        studies["Duration"]
        .apply(lambda x: "/".join(re.findall(r"[\d./-]+", x)))
        .tolist()
    )

    # Extract administration_term_units column
    administration_term_units = (
        studies["Duration"]
        .apply(lambda x: "/".join(re.findall(r"[a-zA-Z]+", x)))
        .tolist()
    )

    # Extract fasting_period column
    fasting_periods = studies["Concomitants"].tolist()

    # Extract curator_comment column
    curator_comments = studies["Comments"].tolist()

    return {
        "id": ids,
        "fk_reference_document_id": fk_reference_document_ids,
        "test_substance_name": test_substance_names,
        "dose_level": dose_levels,
        "dose_level_units": dose_level_units,
        "administration_route": administration_routes,
        "administration_term": administration_terms,
        "administration_term_units": administration_term_units,
        "fasting_period": fasting_periods,
        "curator_comment": curator_comments,
    }


def extract_subjects_sheet(df):
    """
    Extract the relevant PharmaPendium columns, and convert them to CvTDB template format

    Args:
        df (pandas.DataFrame): The PharmaPendium file as a DataFrame

    Returns:
        dict: The extracted CvTDB fields
    """
    subjects = df.groupby(["Document"].extend(subjects_group), dropna=False).head(1)
    num_subjects = len(subjects)

    # Extract id column
    ids = [i for i in range(1, num_subjects + 1)]

    # Extract species column
    species = subjects["Species"].tolist()

    # Extract sex column
    sexs = subjects["Sex"].tolist()

    # Extract age_category column
    age_categories = subjects["Age"].tolist()

    return {
        "id": ids,
        "species": species,
        "sex": sexs,
        "age_category": age_categories,
    }


def extract_series_sheet(df):
    """
    Extract the relevant PharmaPendium columns, and convert them to CvTDB template format

    Args:
        df (pandas.DataFrame): The PharmaPendium file as a DataFrame

    Returns:
        dict: The extracted CvTDB fields
    """
    series = df.groupby(
        [
            "Document",
        ].extend(series_group),
        dropna=False,
    ).head(1)
    num_series = len(series)

    # Extract id column
    ids = [i for i in range(1, num_series + 1)]

    # Extract analyte_name column
    analyte_names = series["Drug"].tolist()

    # Extract time_units column
    time_units = (
        series["t"]
        .astype(str)
        .apply(lambda x: "/".join(re.findall(r"[a-zA-Z]+", x)))
        .tolist()
    )

    # Extract conc_units column
    conc_units = series["Units"]

    # Extract analytical_method_detail column
    analytical_method_details = series["Assay"].tolist()

    # Extract radiolabeled column
    radiolabels = (
        series["Radiolabelled"]
        .astype(str)
        .apply(lambda x: 0 if x == "Not radiolabelled" else 1)
    )

    # Extract fk_study_id column
    studies = (
        df.groupby(["Document"].extend(studies_group), dropna=False)
        .head(1)
        .reset_index()
    )
    studies["id"] = studies.index + 1
    fk_study_ids = series.merge(
        studies,
        how="inner",
        left_on=studies_group,
        right_on=studies_group,
    )["id"].tolist()

    # Extract fk_subject_ids column
    subjects = (
        df.groupby(["Document"].extend(subjects_group), dropna=False)
        .head(1)
        .reset_index()
    )
    subjects["id"] = subjects.index + 1

    fk_subject_ids = series.merge(
        subjects,
        how="inner",
        left_on=subjects_group,
        right_on=subjects_group,
    )["id"].tolist()

    # Extract curator_comment column
    curator_comments = series["Parameter"].tolist()

    return {
        "id": ids,
        "analyte_name": analyte_names,
        "time_units": time_units,
        "conc_units": conc_units,
        "analytical_method_detail": analytical_method_details,
        "radiolabeled": radiolabels,
        "fk_study_id": fk_study_ids,
        "fk_subject_id": fk_subject_ids,
        "curator_comment": curator_comments,
    }


def extract_conc_time_values_sheet(df):
    """
    Extract the relevant PharmaPendium columns, and convert them to CvTDB template format

    Args:
        df (pandas.DataFrame): The PharmaPendium file as a DataFrame

    Returns:
        dict: The extracted CvTDB fields
    """
    # Extract fk_series_id column
    series = (
        df.groupby(
            ["Document"].extend(series_group),
            dropna=False,
        )
        .head(1)
        .reset_index()
    )
    series["id"] = series.index + 1
    fk_series_ids = df.merge(
        series,
        how="inner",
        left_on=series_group,
        right_on=series_group,
    )["id"].tolist()

    # Extract time column
    times = (
        df["t"]
        .astype(str)
        .apply(lambda x: "/".join(re.findall(r"[\d./-]+", x)))
        .tolist()
    )

    # Extract conc column
    concs = df["Value"]

    # Extract conc_sd column
    conc_sds = df["SD"]

    return {
        "fk_series_id": fk_series_ids,
        "time": times,
        "conc": concs,
        "conc_sd": conc_sds,
    }


def create_template(df, template_name, in_template):
    """
    Creates the CvTDB template using the extracted information from the PharmaPendium file

    Args:
        df (pd.DataFrame): A PharmaPendium DataFrame grouped by a Document
        template_name (str): The full name path for the output template
        in_template (pd.DataFrame): The blank input CvTDB template
    """
    for sheet_name in in_template.keys():
        # Call extraction function dynamically
        s_df = pd.DataFrame(globals()[f"extract_{sheet_name.lower()}_sheet"](df))
        # Find missing cols
        s_missing = list(set(in_template[sheet_name].columns) - set(s_df.columns))
        s_df[s_missing] = np.nan
        # Check if file exists, write new or append
        if not os.path.isfile(template_name):
            # print(f"Writing new XLSX for {sheet_name}")
            with pd.ExcelWriter(template_name, engine="openpyxl") as writer:
                # Re-order fields and append to XLSX
                s_df[in_template[sheet_name].columns.tolist()].to_excel(
                    writer, sheet_name=sheet_name, index=False
                )
        else:
            # print(f"Appending to XLSX for {sheet_name}")
            with pd.ExcelWriter(template_name, engine="openpyxl", mode="a") as writer:
                # Re-order fields and append to XLSX
                s_df[in_template[sheet_name].columns.tolist()].to_excel(
                    writer, sheet_name=sheet_name, index=False
                )


if __name__ == "__main__":
    folder_path = os.getenv("in_dir")
    dest_path = os.getenv("out_dir")

    file_paths = os.listdir(folder_path)

    # Load CvT Template (help fill in columns)
    cvt_template = pd.read_excel("input/CvT_data_template_articles.xlsx", None)

    for file_path in file_paths:
        try:
            # get the main document
            with warnings.catch_warnings(record=True):
                warnings.simplefilter("always")
                df = pd.read_excel(
                    os.path.join(folder_path, file_path), engine="openpyxl"
                )

            # Filter out all documents that contain concomitants
            df = df[
                df["Concomitants"]
                .astype(str)
                .str.lower()
                .isin(["fed", "fasted", "nan"])
            ]
            df["Document"] = (
                df["Source Link"].astype(str).apply(lambda x: x.split("?", 1)[0])
            )
            df["Source Split"] = (
                df["Source"].astype(str).apply(lambda x: x.split(",")[0])
            )

            # create a list of dataframes, one for each group
            document_groups = df.groupby("Document")
            doc_dfs = []

            for name, group in document_groups:
                doc_dfs.append(group)

            for i, doc_df in enumerate(doc_dfs):
                filename = doc_df["filename"].head(1).values[0]
                if filename is None:
                    filename = (
                        "%030x" % random.randrange(16**30) + ".pdf"
                    )  # Random hexstring for filename
                template_name = str(filename).replace(".pdf", ".xlsx")
                create_template(
                    doc_df, os.path.join(dest_path, template_name), cvt_template
                )
        except:
            print(f"error at filepath {file_path}")
