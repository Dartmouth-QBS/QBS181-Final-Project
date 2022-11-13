import tabula
import pandas as pd
import os

# Directory containing PDFs from City of Chicago website
directory = "./pdfs/"

# All files in directory, excluding hidden files
list_of_files = [f for f in os.listdir(directory) if not f.startswith('.')]

# Empty dictionary to be populated with neighborhoods as keys and race demographics as values
all_data = {}

# Extract table from PDF
for file in list_of_files:
    print(file)
    neighborhood = file[0:-4].replace("+", " ")
    pdf_data = tabula.read_pdf(os.path.join(directory, file), pages="11")[0]
    if (pdf_data.shape[1] == 4):
        race_data = pdf_data.iloc[1:6, 0:3]
        race_data.columns = ["Race", "2000", "2006-2010"]
    else:
        raise Exception("INCORRECT NUMBER OF COLUMNS")
    race_data["Neighborhood"] = neighborhood
    all_data[neighborhood] = race_data

concatenated_race_data = pd.concat(all_data.values(), ignore_index = True)
unique_neighborhoods =  sorted(concatenated_race_data.Neighborhood.unique(), key=str.casefold)
#print(concatenated_race_data)
#print(unique_neighborhoods)
#print(concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[4::5, 0].values)
all_race_data = {"Neighborhood": unique_neighborhoods,
"White (Non-Hispanic) 2000": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[4::5, 0].values,
"White (Non-Hispanic) 2006-2010": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2006-2010"]].agg(" ".join).iloc[4::5, 0].values,
"Black (Non-Hispanic) 2000": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[1::5, 0].values,
"Black (Non-Hispanic) 2006-2010": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2006-2010"]].agg(" ".join).iloc[1::5, 0].values,
"Hispanic or Latino (of Any Race) 2000": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[2::5, 0].values,
"Hispanic or Latino (of Any Race) 2006-2010": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2006-2010"]].agg(" ".join).iloc[2::5, 0].values,
"Asian (Non-Hispanic) 2000": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[::5, 0].values,
"Asian (Non-Hispanic) 2006-2010": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2006-2010"]].agg(" ".join).iloc[::5, 0].values,
"Other/Multiple Races (Non-Hispanic) 2000": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2000"]].agg(" ".join).iloc[3::5, 0].values,
"Other/Multiple Races (Non-Hispanic) 2006-2010": concatenated_race_data.groupby(["Neighborhood", "Race"])[["2006-2010"]].agg(" ".join).iloc[3::5, 0].values}

final_df = pd.DataFrame(all_race_data)

final_df.to_csv("all_race_data.csv", index = False)