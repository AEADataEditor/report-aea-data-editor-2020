Processing Figures and Tables
=============================
The programs in this folder create the figures and tables in the article. They need to be run before compiling the LaTeX. No post-processing should be needed.

Data sources
------------
- `data/jira`: 
   - `confidential`: Internal data from the JIRA system used by the AEA Data Editor. Not provided.
   - `anon`: Anonymized subset of the internal data used to generate all figures and tables. Download data from Vilhuber(2020c) and save into this folder. `04_jira_anonymize.R` was used to anonymize these data.
- `data/rct`
   - `raw`: Raw data as downloaded from the internal systems at the AEA RCT Registry by James Turitto. Since providing this data, the AEA RCT Registry now provides snapshots of the data at [https://dataverse.harvard.edu/dataverse/aearegistry](https://dataverse.harvard.edu/dataverse/aearegistry). The data here would be similar to AEA RCT Registry(2020), though that has not been verified.
   - `generated`: Simplified data, based on the raw data. Provided as part of this archive. Created by `01_prepare-rct.R`. 
- `externals/aea-supplement-migration/`: Data and Figures from Vilhuber (2020b) and https://github.com/AEADataEditor/aea-supplement-migration. 
   - Download the relevant release (corrected version: v20200515, URL: https://github.com/AEADataEditor/aea-supplement-migration/archive/v20200515.zip), unpack into `externals/`
   - `programs/`: Source for figures copied to and provided as part of the `images` directory in this repository. Copied using `03_copy_migration_figures.R`
   - `data/generated/`: Source for tables, copied using `06_copy_migration_tables.R` and provided as part of this repository. 
- `tables`: This directory contains data copied using `06_copy_migration_tables.R` and serves as an input to processing in `07_print_migration_tables.R`

Pre-requisites
--------------
- R (last run with 3.6.1)
  - Package `here` is required
  - all other packages are listed at the top of each R program, and will be installed automatically
- Bash shell (optional)
  - used to run all R scripts in sequence

Processing
----------

- run `run_all.sh` from a shell to process all files in numerical order
- Note that in the absence of the raw (confidential) data files used in `01_prepare-rct.R` and `04_jira_anonymize.R`, those step will be skipped. This is normal.

Outputs
-------
Output folders are listed in `config.R`. By default, 
 - figures are in the `(BASEPATH)/images` folder
 - tables (and CSV files) are in the `(BASEPATH)/tables` folder

### Mapping tables and figures to article

| Name of file | Figure/ Table in article | Program to create |
|--------------|--------------------------|-------------------|
|Screenshot_aer_data_citation.png | Figure 1 | Manually created |
| figure_migration_files.png | Figure 2 | `03_copy_migration_figures.R`| 
| figure_migration_doi_by_year.png | Figure 3 | `03_copy_migration_figures.R`| 
| figure_migration_software_pkgs.png | Figure 4 | `03_copy_migration_figures.R`| 
| figure_migration_software_years_pct.png | Figure  5| `03_copy_migration_figures.R`| 
| n_assessments_journal_plot.png | Figure 6 | `05_jira_stats_graphs.R`| 
| n_rounds_plot.png | Figure 7 | `05_jira_stats_graphs.R`| 
| revision_round_length_hist.png | Figure  8| `05_jira_stats_graphs.R`| 
| total_length_hist.png | Figure 9| `05_jira_stats_graphs.R`| 
| author_response_hist.png | Figure 10 | `05_jira_stats_graphs.R`| 
| figure_rctgrowth.png | Figure 11 | `02_analysis-rct.R`| 
| figure_preregistrations.png | Figure 12 | `02_analysis-rct.R`| 
| figure_preanalysisplans.png | Figure 13 | `02_analysis-rct.R`| 
| table_software.tex | Table 1 | `07_print_migration_tables.R` |

Changelog
---------
After publication, Alan Riley (Stata) pointed out a discrepancy between Table 1 and Figure 5, which should be depicting (on average) the same data. This was corrected in the code associated with the AEA Repository migration (see https://github.com/AEADataEditor/aea-supplement-migration/commit/530f1e9ad8059e68815b5836db33155c990154b0 for the commit implementing the code change).

References
----------
AEA RCT Registry. 2020. “Registrations   in   the   AEA   RCT   Registry   (2013-04 to 2020-01).” Harvard Dataverse UNF:6:kWiM5wm1x75KKsxWAAqr4g==, https://doi.org/10.7910/DVN/DFMLIU.

Vilhuber, Lars. 2020b. "Data and code for:Data files for AEA Repository migration" American Economic Association [publisher], Inter-university Consortium for Political and Social Research [distributor]. http://doi.org/10.3886/E117873V1

Vilhuber,  Lars. 2020c.  “Process  data  for  the AEA  Pre-publication  Verification  Service.” American Economic Association [publisher],https://doi.org/10.3886/E117876V1.


