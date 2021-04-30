# 2020 Report by the AEA Data Editor

This repository contains the code, data, and manuscript files for the 2020 report by the AEA Data Editor. If you are reading this on openICPSR, then only code and data are present.


## Locations

The repository at [https://github.com/AEADataEditor/report-aea-data-editor-2020](https://github.com/AEADataEditor/report-aea-data-editor-2020) contains text, code, data, and output from running the code. 

The deposit at [https://doi.org/10.3886/E135023V1](https://doi.org/10.3886/E135023V1) contains code and data, as well as output. 


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

## Computational requirements


### Software Requirements


- R 4.0.1
  - Package versions set to 2021-01-01, using MRAN, except for Github installed versions
  - `tidyr` (0.8.3)
  - `rdrobust` (0.99.4)
  - the file "`0_setup.R`" will install all dependencies (latest version), and should be run once prior to running other programs.

## Running code

See the `programs/README.md` file for further details.

Two additional utility scripts are present, for packaging up the contents of the git repository. They are not needed to either create figures and tables, or to compile the article.


### Programs

All programs are in the `programs` subdirectory:
```
programs/01_jira_anonymize.R
programs/02_jira_anon_publish.R
programs/03_jira_dataprep.R
programs/04_table1_compliance.R
programs/05_table2_stats.R
programs/06_table3_response_options.R
programs/07_table4.R
programs/08_figure1_filesize.R
programs/09_write_nums.R
programs/README.md
programs/config.R
programs/run_all.sh
```
See the `programs/README.md` file for further details.

### Figures and Images
All images, whether hand-created or generated, are in the `images` directory:

```
images/plot_filesize_dist.png
```

To map these to the relevant figures in the article, see `programs/README.md`  for further details.

## References


- Vilhuber,  Lars. 2021a.  “Process  data  for  the AEA  Pre-publication  Verification  Service.” American Economic Association [publisher], https://doi.org/10.3886/E117876V2