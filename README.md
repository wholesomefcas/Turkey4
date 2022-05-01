# Boston

The input folder contains all the raw pdf, OCR scanned text file reference articles, and dictionary used in this project. The data in this project can be accessed from https://dhsprogram.com/publications/publication-fr108-dhs-final-reports.cfm.

The output folder contains the PDF version of the report, the reference file, cleaned data, the graph used in the project, and the code for the graph, data analysis, result and discussion part of this project 

The script folder contain the data cleaning code for the project. To be specific:

‘scripts/01-gather_data.R’ OCR the PDF and save the output to ‘outputs/data/raw_data.csv’

‘scripts/02-clean_and_prepare_data.R’ draws on ‘raw_data.csv’ to clean and prepare the dataset. Save the dataset to ‘outputs/data/cleaned_data.csv’.
