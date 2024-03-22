import os
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

    # Get the indices of non-empty qc_reviewer_lanid in revised_df_full
    non_empty_indices = revised_df_full[
        revised_df_full["qc_reviewer_lanid"].notnull()
    ].index

    # Update revised_df_full and original_df_full with the rows matching the non-empty indices
    original_df_full = original_df_full.loc[non_empty_indices]
    revised_df_full = revised_df_full.loc[non_empty_indices]

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
    reviewed_rows = revised_df.qc_reviewer_lanid.count()

    duplicated_rows_percentage = round((duplicated_rows / original_rows) * 100, 2)
    non_duplicated_rows_percentage = round(row_count / original_rows * 100, 2)
    reviewed_rows_percentage = round(reviewed_rows / row_count * 100, 2)

    # Create a dictionary with the data
    data = {
        "Count": [original_rows, duplicated_rows, row_count, reviewed_rows, ""],
        "Percentage": [
            "",
            duplicated_rows_percentage,
            non_duplicated_rows_percentage,
            reviewed_rows_percentage,
            "",
        ],
    }

    # Create a DataFrame
    row_count_df = pd.DataFrame(
        data,
        index=[
            "Original Rows",
            "Duplicated Rows",
            "Non-Duplicated Rows",
            "Reviewed Rows",
            "Adjusted Rows",
        ],
    )

    # Compare the original dataframe to the revised dataframe using the original columns
    compared_df = original_df.compare(
        revised_df[original_columns], keep_equal=False, keep_shape=True
    )
    compared_columns = compared_df.columns.tolist()

    changed_data = compared_df.loc[:, pd.IndexSlice[:, "self"]].dropna(how="all")
    new_data = compared_df.loc[:, pd.IndexSlice[:, "other"]].dropna(how="all")

    # Add to row count df
    row_count_df.at["Adjusted Rows", "Count"] = len(changed_data)
    row_count_df.at["Adjusted Rows", "Percentage"] = round(
        len(changed_data) / row_count * 100, 2
    )

    # Create a new dataframe to store the percentages
    percentage_df = pd.DataFrame(
        columns=[
            "Column",
            "Changes_Count",
            "Changes_Percentage",
            "Additions_Count",
            "Additions_Percentage",
        ]
    )

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
                "Changes_Percentage": [round(percentage_changes, 2)],
                "Changes_Count": [changes],
                "Additions_Percentage": [round(percentage_new_additions, 2)],
                "Additions_Count": [new_additions],
            }
        )

        # Append the percentages to the new dataframe
        percentage_df = pd.concat([percentage_df, new_df], ignore_index=True)

    percentage_df.set_index("Column", inplace=True)

    additions_count = total_rows - revised_df[added_columns].isnull().sum()
    additions_percentage = round((100 * additions_count) / total_rows, 2)

    new_column_df = pd.DataFrame(columns=["Additions_Count", "Additions_Percentage"])
    new_column_df["Additions_Percentage"] = additions_percentage
    new_column_df["Additions_Count"] = additions_count

    new_column_df.index.name = "Column"

    return row_count_df, percentage_df, new_column_df


if __name__ == "__main__":
    input_path = "inputs"

    input_files = os.listdir(input_path)
    original_files = [
        input_file
        for input_file in input_files
        if "Pharmacokinetic_Concentration" in input_file
    ]
    revised_files = [
        input_file
        for input_file in input_files
        if "Pharmacokinetic_Concentration" not in input_file
    ]

    with pd.ExcelWriter("comparison_results.xlsx", engine="xlsxwriter") as writer:
        # Initialize the total_count sheets
        total_row_count_df = pd.DataFrame()
        total_percentage_df = pd.DataFrame()
        total_new_column_df = pd.DataFrame()

        # Write the combined and normalized dataframes to Excel
        total_row_count_df.to_excel(writer, sheet_name="row_counts")
        total_percentage_df.to_excel(writer, sheet_name="original_data")
        total_new_column_df.to_excel(writer, sheet_name="new_data")

        # Create the individual sheets
        print("Gathering data from files..")
        for original_file in original_files:
            file_identifier = original_file.split("_")[0]
            revised_file = next(
                revised_file
                for revised_file in revised_files
                if file_identifier in revised_file
            )
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
                writer, sheet_name=f"{file_identifier}_original_data"
            )
            new_column_df.to_excel(writer, sheet_name=f"{file_identifier}_new_data")

        # Normalize the combined sheets
        file_length = len(original_files)

        total_row_count_df["Percentage"] = (
            total_row_count_df["Percentage"]
            .replace({"": 0})
            .astype(float)
            .div(file_length)
            .round(1)
        )
        total_percentage_df["Changes_Percentage"] = (
            total_percentage_df["Changes_Percentage"]
            .astype(float)
            .div(file_length)
            .round(1)
        )
        total_percentage_df["Additions_Percentage"] = (
            total_percentage_df["Additions_Percentage"]
            .astype(float)
            .div(file_length)
            .round(1)
        )
        total_new_column_df["Additions_Percentage"] = (
            total_new_column_df["Additions_Percentage"].div(file_length).round(1)
        )

        # Sort Columns
        total_percentage_df.sort_values(
            by=["Changes_Percentage", "Additions_Percentage"],
            ascending=[False, False],
            inplace=True,
        )
        total_new_column_df.sort_values(
            by=["Additions_Percentage"], ascending=[False], inplace=True
        )

        # Clean Columns
        total_row_count_df.Percentage = total_row_count_df.Percentage.astype(str) + "%"
        total_percentage_df.Changes_Percentage = (
            total_percentage_df.Changes_Percentage.astype(str) + "%"
        )
        total_percentage_df.Additions_Percentage = (
            total_percentage_df.Additions_Percentage.astype(str) + "%"
        )
        total_new_column_df.Additions_Percentage = (
            total_new_column_df.Additions_Percentage.astype(str) + "%"
        )

        # Write the combined and normalized dataframes to Excel
        total_row_count_df.to_excel(writer, sheet_name="row_counts")
        total_percentage_df.to_excel(writer, sheet_name="original_data")
        total_new_column_df.to_excel(writer, sheet_name="new_data")

        # Update the column widths in Excel
        for sheetname in writer.sheets:
            worksheet = writer.sheets[sheetname]
            worksheet.set_column(0, 0, 20)

            if "new" in sheetname:
                worksheet.set_column(1, 1, 20)
