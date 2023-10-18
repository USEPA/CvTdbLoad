import pandas as pd

file_path = 'example_file_name.xlsx'

duplicated_columns = ['Age', 'Dose', 'Duration', 'Value', 'Parameter', 'Parameter Value', 'Value', 'Units', 'SD', 't']

df = pd.read_excel(file_path, index_col=0)
df = df.sort_values('Year')
df['duplicate'] = 'false'
df.loc[df.duplicated(subset=duplicated_columns), 'duplicate'] = 'true'
df.to_excel(file_path.replace('.xlsx', '_revised.xlsx'))