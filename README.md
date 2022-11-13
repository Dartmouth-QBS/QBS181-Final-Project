# Racial Disparities in Women's Health in Chicago

## QBS 181 Final Project, Fall 2022

**To access final report:** [https://rstudio-connect-dev.dartmouth.edu/content/dde20d72-c179-40f4-b008-8a7db9c7f0f1/Final_Writeup.html](https://rstudio-connect-dev.dartmouth.edu/content/dde20d72-c179-40f4-b008-8a7db9c7f0f1/Final_Writeup.html)

### File Structure Explanation
-   **data/**: directory containing all datasets used in project

    -   **health_stats/**: contains all public health data, as collected from [Kaggle](https://www.kaggle.com/datasets/chicago/chicago-public-health-statistics)
    -   **race_data/**: contains race demographic data as collected from parsed data from [CMAP](https://www.cmap.illinois.gov/data/community-snapshots)

-   **excel_files/**: directory containing Excel files to clean data prior to loading into R

-   **python_pipelines/**: directory containing 2 Python scripts to scrape and parse demographic data from [CMAP](https://www.cmap.illinois.gov/data/community-snapshots)

    -   **pdfs/**: directory with PDFs from each of the 77 neighborhoods in Chicago directly downloaded from CMAP
    -   **parse.py**: web scraping script to download PDFs from CMAP
    -   **race_data_pipeline.py**: script to parse needed data from each CMAP PDF

-   **source_code/**: directory with all backend scripts for statistical analysis and visualizations that are called in the final Rmd file

    -   **stat_analysis/**: all one-way ANOVA and Tukey-Kramer HSD Post-Hoc tests for each dataset analyzed

-   **Final_Writeup.Rmd**: R Markdown file containing all visualizations, analyses, and discussions
