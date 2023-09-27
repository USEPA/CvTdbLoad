from dotenv import load_dotenv
import pandas as pd
import os
import random
import re
import warnings

load_dotenv(".env")


def extract_documents_sheet(df):
    documents = df.groupby(["Document"], dropna=False).head(1)
    num_documents = len(documents)

    # Extract id column
    ids = [i for i in range(1, num_documents + 1)]

    # Extract document_type column
    document_types = [1] * num_documents

    # Extract pmid column
    pmids = [""] * num_documents

    # Extract other_study_identifier columns
    other_study_identifiers = documents["Study Number"].tolist()

    # Extract extracted column
    extracteds = [""] * num_documents

    # Extract doi column
    source_links = documents["Source Link"].astype(str).tolist()
    dois = [
        x.replace("http://dx.doi.org/", "") if "doi" in x else None
        for x in source_links
    ]

    # Extract first_author column
    first_authors = [""] * num_documents

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
        "pmid": pmids,
        "other_study_identifier": other_study_identifiers,
        "extracted": extracteds,
        "doi": dois,
        "first_author": first_authors,
        "year": years,
        "title": titles,
        "url": urls,
        "curator_comment": curator_comments,
    }


def extract_studies_sheet(df):
    studies = df.groupby(
        ["Document", "Drug", "Dose", "Duration", "Route"], dropna=False
    ).head(1)
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

    # Extract test_substance_name_secondary column
    test_substance_name_secondaries = [""] * num_studies

    # Extract test_substance_casrn column
    test_substance_casrns = [""] * num_studies

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

    # Extract dose_duration column
    dose_durations = [""] * num_studies

    # Extract dose_duration_units column
    dose_duration_units = [""] * num_studies

    # Extract dose_frequency column
    dose_frequencies = [""] * num_studies

    # Extract dose_vehicle column
    dose_vehicles = [""] * num_studies

    # Extract dose_volume column
    dose_volumes = [""] * num_studies

    # Extract dose_volume_units column
    dose_volume_units = [""] * num_studies

    # Extract fasting_period column
    fasting_periods = studies["Concomitants"].tolist()

    # Extract author_comment column
    author_comments = [""] * num_studies

    # Extract curator_comment column
    curator_comments = studies["Comments"].tolist()

    # Extract dermal_dose_vehicle_pH column
    dermal_dose_vehicle_pHs = [""] * num_studies

    # Extract dermal_applied_area column
    dermal_applied_areas = [""] * num_studies

    # Extract dermal_applied_area_units column
    dermal_applied_area_units = [""] * num_studies

    # Extract aerosol_particle_diameter_mean column
    aerosol_particle_diameter_mean = [""] * num_studies

    # Extract aerosol_particle_diameter_gsd column
    aerosol_particle_diameter_gsd = [""] * num_studies

    # Extract aerosol_particle_diameter_units column
    aerosol_particle_diameter_units = [""] * num_studies

    # Extract aerosol_particle_density column
    aerosol_particle_densities = [""] * num_studies

    # Extract aerosol_particle_density_units column
    aerosol_particle_density_units = [""] * num_studies

    return {
        "id": ids,
        "fk_reference_document_id": fk_reference_document_ids,
        "test_substance_name": test_substance_names,
        "test_substance_name_secondary": test_substance_name_secondaries,
        "test_substance_casrn": test_substance_casrns,
        "dose_level": dose_levels,
        "dose_level_units": dose_level_units,
        "administration_route": administration_routes,
        "administration_term": administration_terms,
        "administration_term_units": administration_term_units,
        "dose_duration": dose_durations,
        "dose_duration_units": dose_duration_units,
        "dose_frequency": dose_frequencies,
        "dose_vehicle": dose_vehicles,
        "dose_volume": dose_volumes,
        "dose_volume_units": dose_volume_units,
        "fasting_period": fasting_periods,
        "author_comment": author_comments,
        "curator_comment": curator_comments,
        "dermal_dose_vehicle_pH": dermal_dose_vehicle_pHs,
        "dermal_applied_area": dermal_applied_areas,
        "dermal_applied_area_units": dermal_applied_area_units,
        "aerosol_particle_diameter_mean": aerosol_particle_diameter_mean,
        "aerosol_particle_diameter_gsd": aerosol_particle_diameter_gsd,
        "aerosol_particle_diameter_units": aerosol_particle_diameter_units,
        "aerosol_particle_density": aerosol_particle_densities,
        "aerosol_particle_density_units": aerosol_particle_density_units,
    }


def extract_subjects_sheet(df):
    subjects = df.groupby(["Document", "Species", "Sex", "Age"], dropna=False).head(1)
    num_subjects = len(subjects)

    # Extract id column
    ids = [i for i in range(1, num_subjects + 1)]

    # Extract species column
    species = subjects["Species"].tolist()

    # Extract subtype column
    subtypes = [""] * num_subjects

    # Extract sex column
    sexs = subjects["Sex"].tolist()

    # Extract age column
    ages = [""] * num_subjects

    # Extract age_units column
    age_units = [""] * num_subjects

    # Extract age_category column
    age_categories = subjects["Age"].tolist()

    # Extract health_status column
    health_statuses = [""] * num_subjects

    # Extract height column
    heights = [""] * num_subjects

    # Extract height_units column
    height_units = [""] * num_subjects

    # Extract weight column
    weights = [""] * num_subjects

    # Extract weight_units column
    weight_units = [""] * num_subjects

    # Extract curator_comment column
    curator_comments = [""] * num_subjects

    return {
        "id": ids,
        "species": species,
        "subtype": subtypes,
        "sex": sexs,
        "age": ages,
        "age_units": age_units,
        "age_category": age_categories,
        "health_status": health_statuses,
        "height": heights,
        "height_units": height_units,
        "weight": weights,
        "weight_units": weight_units,
        "curator_comment": curator_comments,
    }


def extract_series_sheet(df):
    series = df.groupby(
        [
            "Document",
            "Parameter",
            "Units",
            "Species",
            "Sex",
            "Age",
            "Dose",
            "Duration",
            "Route",
        ],
        dropna=False,
    ).head(1)
    num_series = len(series)

    # Extract id column
    ids = [i for i in range(1, num_series + 1)]

    # Extract analyte_name column
    analyte_names = series["Drug"].tolist()

    # Extract analyte_name_secondary column
    analyte_name_secondaries = [""] * num_series

    # Extract analyte_casrn column
    analyte_casrns = [""] * num_series

    # Extract figure_name column
    figure_names = [""] * num_series

    # Extract figure_type column
    figure_types = [""] * num_series

    # Extract figure_series_identifier column
    figure_series_identifiers = [""] * num_series

    # Extract x_min column
    x_mins = [""] * num_series

    # Extract x_max column
    x_maxs = [""] * num_series

    # Extract y_min column
    y_mins = [""] * num_series

    # Extract y_max column
    y_maxs = [""] * num_series

    # Extract time_units column
    time_units = (
        series["t"]
        .astype(str)
        .apply(lambda x: "/".join(re.findall(r"[a-zA-Z]+", x)))
        .tolist()
    )

    # Extract conc_units column
    conc_units = series["Units"]

    # Extract conc_cumulative column
    conc_cumulatives = [""] * num_series

    # Extract log_conc_units column
    log_conc_units = [""] * num_series

    # Extract loq column
    loqs = [""] * num_series

    # Extract loq_units column
    loq_units = [""] * num_series

    # Extract lod column
    lods = [""] * num_series

    # Extract lod_units column
    lod_units = [""] * num_series

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
        df.groupby(["Document", "Drug", "Dose", "Duration", "Route"], dropna=False)
        .head(1)
        .reset_index()
    )
    studies["id"] = studies.index + 1
    fk_study_ids = series.merge(
        studies,
        how="inner",
        left_on=["Drug", "Dose", "Duration", "Route"],
        right_on=["Drug", "Dose", "Duration", "Route"],
    )["id"].tolist()

    # Extract fk_subject_ids column
    subjects = (
        df.groupby(["Document", "Species", "Sex", "Age"], dropna=False)
        .head(1)
        .reset_index()
    )
    subjects["id"] = subjects.index + 1

    fk_subject_ids = series.merge(
        subjects,
        how="inner",
        left_on=["Species", "Sex", "Age"],
        right_on=["Species", "Sex", "Age"],
    )["id"].tolist()

    # Extract n_subjects_in_series column
    n_subjects_in_series = [""] * num_series

    # Extract conc_medium column
    conc_mediums = [""] * num_series

    # Extract curator_comment column
    curator_comments = series["Parameter"].tolist()

    return {
        "id": ids,
        "analyte_name": analyte_names,
        "analyte_name_secondary": analyte_name_secondaries,
        "analyte_casrn": analyte_casrns,
        "figure_name": figure_names,
        "figure_type": figure_types,
        "figure_series_identifier": figure_series_identifiers,
        "x_min": x_mins,
        "x_max": x_maxs,
        "y_min": y_mins,
        "y_max": y_maxs,
        "time_units": time_units,
        "conc_units": conc_units,
        "conc_cumulative": conc_cumulatives,
        "log_conc_units": log_conc_units,
        "loq": loqs,
        "loq_units": loq_units,
        "lod": lods,
        "lod_units": lod_units,
        "analytical_method_detail": analytical_method_details,
        "radiolabeled": radiolabels,
        "fk_study_id": fk_study_ids,
        "fk_subject_id": fk_subject_ids,
        "n_subjects_in_series": n_subjects_in_series,
        "conc_medium": conc_mediums,
        "curator_comment": curator_comments,
    }


def extract_conc_time_values_sheet(df):
    num_conc_time_values = len(df)

    # Extract fk_series_id column
    series = (
        df.groupby(
            [
                "Document",
                "Parameter",
                "Units",
                "Species",
                "Sex",
                "Age",
                "Dose",
                "Duration",
                "Route",
            ],
            dropna=False,
        )
        .head(1)
        .reset_index()
    )
    series["id"] = series.index + 1
    fk_series_ids = df.merge(
        series,
        how="inner",
        left_on=[
            "Parameter",
            "Units",
            "Species",
            "Sex",
            "Age",
            "Dose",
            "Duration",
            "Route",
        ],
        right_on=[
            "Parameter",
            "Units",
            "Species",
            "Sex",
            "Age",
            "Dose",
            "Duration",
            "Route",
        ],
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

    # Extract conc_bound_type column
    conc_bound_types = [""] * num_conc_time_values

    # Extract conc_lower_bound column
    conc_lower_bounds = [""] * num_conc_time_values

    # Extract conc_upper_bound column
    conc_upper_bounds = [""] * num_conc_time_values

    # Extract curator_comment column
    curator_comments = [""] * num_conc_time_values

    return {
        "fk_series_id": fk_series_ids,
        "time": times,
        "conc": concs,
        "conc_sd": conc_sds,
        "conc_bound_type": conc_bound_types,
        "conc_lower_bound": conc_lower_bounds,
        "conc_upper_bound": conc_upper_bounds,
        "curator_comment": curator_comments,
    }


def create_template(df, template_name):
    # Create dataframes for the sheets
    documents_df = pd.DataFrame(extract_documents_sheet(df))
    studies_df = pd.DataFrame(extract_studies_sheet(df))
    subjects_df = pd.DataFrame(extract_subjects_sheet(df))
    series_df = pd.DataFrame(extract_series_sheet(df))
    conc_time_values_df = pd.DataFrame(extract_conc_time_values_sheet(df))

    # Create excel file
    with pd.ExcelWriter(template_name, engine="xlsxwriter") as writer:
        documents_df.to_excel(writer, sheet_name="Documents", index=False)
        studies_df.to_excel(writer, sheet_name="Studies", index=False)
        subjects_df.to_excel(writer, sheet_name="Subjects", index=False)
        series_df.to_excel(writer, sheet_name="Series", index=False)
        conc_time_values_df.to_excel(writer, sheet_name="Conc_Time_Values", index=False)


folder_path = os.getenv("in_dir")
dest_path = os.getenv("out_dir")

file_paths = os.listdir(folder_path)

for file_path in file_paths:
    try:
        # get the main document
        with warnings.catch_warnings(record=True):
            warnings.simplefilter("always")
            df = pd.read_excel(os.path.join(folder_path, file_path), engine="openpyxl")

        # Filter out all documents that contain concomitants
        df = df[
            df["Concomitants"].astype(str).str.lower().isin(["fed", "fasted", "nan"])
        ]
        df["Document"] = (
            df["Source Link"].astype(str).apply(lambda x: x.split("?", 1)[0])
        )
        df["Source Split"] = df["Source"].astype(str).apply(lambda x: x.split(",")[0])

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
            create_template(doc_df, os.path.join(dest_path, template_name))
    except:
        print(f"error at filepath {file_path}")
