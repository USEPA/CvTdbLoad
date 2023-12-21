from dotenv import load_dotenv, find_dotenv
import os
import pandas as pd

load_dotenv(find_dotenv())


def flag_duplicates(file_paths: list):
    """
    Take an input list of Pharmapendium spreadsheets, skip the header rows, and flag rows that are the same with a value in
    a column named "duplicate" with 0 being an original, and 1 being a duplicate. The first instance of a row is considered
    a non-duplicate.

    Args:
        file_paths (list): A list of files from an input directory containing Pharmapendium spreadsheets.
    """
    # If all of these columns are the same, the 2nd instance of the row is considered a duplicate
    duplicated_columns = [
        "Age",
        "Dose",
        "Duration",
        "Value",
        "Parameter",
        "Parameter Value",
        "Units",
        "SD",
        "t",
    ]
    # Various Pharmapendium export files contain descriptive header rows.. skip these
    skiprows = input("Enter the amount of rows before the column labels: ")

    print("Attempting to flag duplicate rows")

    for file_path in file_paths:
        if file_path.endswith(".xlsx"):
            print(f"Flagging rows for file {file_path}..")
            df = pd.read_excel(file_path, index_col=0, skiprows=skiprows)

            # Ensure that each column from the duplicated columns exists in the input columns
            if all(column in df.columns for column in duplicated_columns):
                df.loc[df.duplicated(subset=duplicated_columns), "duplicate"] = "true"
            else:
                print(
                    f"The required columns for {file_path} do not match up.. skipping.."
                )
                continue

            df = df.sort_values("Year")
            df["duplicate"] = "false"
            df.to_excel(file_path.replace(".xlsx", "_revised.xlsx"))
            print(f"Flagging completed for {file_path}.")
        else:
            print("Skipping a non-excel file..")

    print("Duplicate flagging complete!")


if __name__ == "__main__":
    input_dir = os.getenv("input_dir")
    file_paths = os.listdir(input_dir)

    # Ensure that the file_paths are nonempty
    if not file_paths:
        print("You must specify a valid input directory")
    else:
        flag_duplicates(file_paths)
