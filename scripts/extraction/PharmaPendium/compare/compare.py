from os.path import join
import pandas as pd


def process_files(original_file, revised_file):
    original_df_full = pd.read_excel(
        join(input_path, original_file), index_col=0
    ).sort_index()
    revised_df_full = pd.read_excel(
        join(input_path, revised_file), index_col=0
    ).sort_index()

    # Ensure our dataframes are the same length
    revised_df_full = revised_df_full.iloc[: len(original_df_full)]

    # Remove the duplicated rows
    non_duplicated_indices = revised_df_full.index[~revised_df_full.duplicate]
    original_df = original_df_full.loc[non_duplicated_indices]
    revised_df = revised_df_full.loc[non_duplicated_indices]

    # Get a list of the newly added columns
    original_columns = list(original_df.columns)
    revised_columns = set(revised_df.columns)
    added_columns = [x for x in revised_columns if x not in original_columns]

    # Check for mismatched datatypes
    for idx, dtype in enumerate(original_df.dtypes):
        revised_dtype = revised_df[original_columns].dtypes[idx]
        if dtype != revised_dtype:
            print(f"Found mismatched datatypes for {original_df.columns[idx]}..")
            print(f"Datatypes are '{dtype}' and '{revised_dtype}'")

    # Fix the mismatched datatype
    revised_df["SD"] = (
        revised_df[original_columns]["SD"].replace(" ", None).astype(float)
    )

    # Some statistics about the original data
    row_count = len(original_df)
    original_rows = len(original_df_full)
    duplicated_rows = original_rows - row_count
    duplicated_rows_percentage = round((duplicated_rows / original_rows) * 100, 2)
    non_duplicated_rows_percentage = round(row_count / original_rows * 100, 2)

    # Create a dictionary with the data
    data = {
        "Number": [original_rows, duplicated_rows, row_count],
        "Percent": ["", duplicated_rows_percentage, non_duplicated_rows_percentage],
    }

    # Create a DataFrame
    row_count_df = pd.DataFrame(
        data, index=["Original Rows", "Duplicated Rows", "Non-Duplicated Rows"]
    )

    # Compare the original dataframe to the revised dataframe using the original columns
    compared_df = original_df.compare(
        revised_df[original_columns], keep_equal=False, keep_shape=True
    )
    compared_columns = compared_df.columns.tolist()

    changed_data = compared_df.loc[:, pd.IndexSlice[:, "self"]].dropna(how="all")
    new_data = compared_df.loc[:, pd.IndexSlice[:, "other"]].dropna(how="all")

    # Create a new dataframe to store the percentages
    percentage_df = pd.DataFrame(columns=["Column", "Changes", "Additions"])

    # Calculate the total number of rows
    total_rows = len(original_df)

    for column, _ in compared_columns[::2]:
        # Calculate the number of changes
        changes = len(changed_data[column].dropna())

        # Calculate the number of new additions
        new_additions = len(new_data[column].dropna()) - changes

        # Calculate the percentages
        percentage_changes = (changes / total_rows) * 100
        percentage_new_additions = (new_additions / total_rows) * 100

        new_df = pd.DataFrame(
            {
                "Column": [column],
                "Changes": [round(percentage_changes, 2)],
                "Additions": [round(percentage_new_additions, 2)],
            }
        )

        # Append the percentages to the new dataframe
        percentage_df = pd.concat([percentage_df, new_df], ignore_index=True)

    percentage_df.set_index("Column", inplace=True)

    added_values = total_rows - revised_df[added_columns].isnull().sum()

    new_column_df = pd.DataFrame(
        columns=["Percentage_Added"], data=round((100 * added_values) / total_rows, 2)
    )
    new_column_df.index.name = "Column"
    # new_column_df.Percentage_Added = new_column_df.Percentage_Added.astype(str) + "%"

    return row_count_df, percentage_df, new_column_df


if __name__ == "__main__":
    input_path = "inputs"
    files = [
        (
            "Rabeprazole_Na_Pharmacokinetic_Concentration_Data.xlsx",
            "65132474e4b0d99f5a8b71f9_CvT_QC_bkesic.xlsx",
            "Rabeprazole",
        ),
        (
            "Tolcapone_Pharmacokinetic_Concentration_Data.xlsx",
            "65132474e4b0d99f5a8b71fe_CvT_QC_rcasey01.xlsx",
            "Tolcapone",
        ),
    ]

    with pd.ExcelWriter("comparison_results.xlsx", engine="xlsxwriter") as writer:
        # Initialize the total_count sheets
        total_row_count_df = pd.DataFrame()
        total_percentage_df = pd.DataFrame()
        total_new_column_df = pd.DataFrame()

        # Write the combined and normalized dataframes to Excel
        total_row_count_df.to_excel(writer, sheet_name="complete_row_counts")
        total_percentage_df.to_excel(
            writer, sheet_name="complete_original_data_changes"
        )
        total_new_column_df.to_excel(writer, sheet_name="complete_new_data_changes")

        # Create the individual sheets
        print("Gathering data from files..")
        for original_file, revised_file, file_identifier in files:
            row_count_df, percentage_df, new_column_df = process_files(
                original_file, revised_file
            )

            if len(total_row_count_df) == 0:
                total_row_count_df = row_count_df
                total_percentage_df = percentage_df
                total_new_column_df = new_column_df
            else:
                total_row_count_df += row_count_df
                total_percentage_df += percentage_df
                total_new_column_df += new_column_df

            row_count_df.to_excel(writer, sheet_name=f"{file_identifier}_row_counts")
            percentage_df.to_excel(
                writer, sheet_name=f"{file_identifier}_original_changes"
            )
            new_column_df.to_excel(writer, sheet_name=f"{file_identifier}_new_changes")

        # Normalize the combined sheets
        file_length = len(files)
        total_row_count_df["Number"] = (
            total_row_count_df["Number"].astype(int).div(file_length)
        )
        total_row_count_df["Percent"] = (
            total_row_count_df["Percent"]
            .replace({"": 0})
            .astype(float)
            .div(file_length)
        )
        total_percentage_df["Changes"] = (
            total_percentage_df["Changes"].astype(float).div(file_length)
        )
        total_percentage_df["Additions"] = (
            total_percentage_df["Additions"].astype(float).div(file_length)
        )
        total_new_column_df["Percentage_Added"] = total_new_column_df[
            "Percentage_Added"
        ].div(file_length)

        # Write the combined and normalized dataframes to Excel
        total_row_count_df.to_excel(writer, sheet_name="complete_row_counts")
        total_percentage_df.to_excel(
            writer, sheet_name="complete_original_data_changes"
        )
        total_new_column_df.to_excel(writer, sheet_name="complete_new_data_changes")

        # Update the column widths in Excel
        for sheetname in writer.sheets:
            worksheet = writer.sheets[sheetname]
            worksheet.set_column(0, 0, 20)

            if "new" in sheetname:
                worksheet.set_column(1, 1, 20)

    print(total_row_count_df)
    print()
    print(total_percentage_df)
    print()
    print(total_new_column_df)
