# 2020 Report by the AEA Data Editor

This repository contains the code, data, and manuscript files for the 2020 report by the AEA Data Editor. If you are reading this on openICPSR, then only code and data are present.

[![DOI](https://zenodo.org/badge/DOI/10.3886/E135023V1.svg)](https://doi.org/10.3886/E135023V1)


## Locations

The repository at [https://github.com/AEADataEditor/report-aea-data-editor-2020](https://github.com/AEADataEditor/report-aea-data-editor-2020) contains text, code, data, and output from running the code. 

The deposit at [https://doi.org/10.3886/E135023V1](https://doi.org/10.3886/E135023V1) contains code and data, as well as output. 

## Citing the report

> Vilhuber, Lars. 2021. "Report by the AEA Data Editor." AEA Papers and Proceedings, 111: 808-17. [https://doi.org/10.1257/pandp.111.808](https://doi.org/10.1257/pandp.111.808)

```
@article{ReportDE2021,
Author = {Vilhuber, Lars},
Title = {Report by the {AEA} Data Editor},
Journal = {AEA Papers and Proceedings},
Volume = {111},
Year = {2021},
Month = {May},
Pages = {808-17},
DOI = {10.1257/pandp.111.808},
URL = {https://www.aeaweb.org/articles?id=10.1257/pandp.111.808}}
```

## Citing the code and data

> Vilhuber, Lars. and Hyuk Son. 2021b. “Code and Data for:  Report for 2020 by the AEA  Data  Editor.”  American Economic Association  [publisher],https://doi.org/10.3886/E135023V1


##  Data

### Summary of Availability

- [ ] All data **are** publicly available.
- [X] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.


### List of Lab members

```
data/replicationlab_members.txt
```

Listed in the article appendix (which takes the data from the above file).

### Data for pre-production verification

Anonymized files from the internal production system are provided in this repository, sourced from Vilhuber (2021a). A copy is provided as part of this archive.

```
data/jira/anon/jira.anon.RDS
data/jira/anon/jira.anon.csv
data/jira/anon/README.md
```

### Data on processing time

The data on processing times were extracted from the ScholarOne manuscript management system used by the AEA. Microdata are not available (even to the author), only summary statistics are provided as Excel sheets. These were simply reformatted for the report.

```
data/scholarone/dataEditorReport_20191128-20201127.xlsx
data/scholarone/README.md
```

## Computational requirements


### Software Requirements

- R 4.0.1
  - Package versions set to as-of 2021-01-01, using MRAN, except for Github installed versions
  - here
  - dplyr
  - tidyr
  - splitstackshape
  - tibble
  - ggplot2
  - remotes
  - stringr
  - readxl
  - "data.table" (github)
  - "Rdatatable" (github)
  - github("markwestcott34/stargazer-booktabs") (overrides standard stargazer!)

Packages are installed within each R program. 


### Programs

All programs are in the `programs` subdirectory:
```
programs/03_jira_dataprep.R
programs/04_table1_compliance.R
programs/05_table2_stats.R
programs/06_table3_response_options.R
programs/07_table4.R
programs/09_write_nums.R
programs/README.md
programs/config.R
programs/run_all.sh
```

### Running code

Each R file can be run independently (separate R sessions), in numerical order. See the `programs/README.md` file for further details.

The script `run_all.sh` is used within a (Linux) shell to implement the above run order, but is optional.

### Mapping tables and figures to article

| Name of file | Figure/ Table in article | Program to create |
|--------------|--------------------------|-------------------|
|n_compliance_manuscript_mod.tex| Table 1 | 04_table1_compliance.R|
|n_journal_numbers_mod.tex| Table 2 | 05_table2_stats.R|
|jira_response_options_mod.tex| Table 3 | 06_table3_response_options.R|
|n_rounds.tex| Table 4 | 07_table4.R|

## License

See LICENSE.txt for data and code license.

## References


- Vilhuber,  Lars. 2021a.  “Process  data  for  the AEA  Pre-publication  Verification  Service.” American Economic Association [publisher], https://doi.org/10.3886/E117876V2