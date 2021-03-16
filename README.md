# 2020 Report by the AEA Data Editor
This is the report for 2020. 

## Published version

> Vilhuber, Lars. 2021. "Report by the AEA Data Editor." AEA Papers and Proceedings, 111: TBD. DOI: [10.1257/pandp.111.TBD](https://doi.org/10.1257/pandp.111.TBD)

## Locations
The repository at [https://github.com/AEADataEditor/report-aea-data-editor-2020](https://github.com/AEADataEditor/report-aea-data-editor-2020) contains text, code, data, and output from running the code. 

The deposit at [https://doi.org/10.3886/E135023V1](https://doi.org/10.3886/E135023V1) contains code and data, as well as output. 


## Citing the code and data

> Vilhuber, Lars. 2021b. “Code and Data for:  Report for 2020 by the AEA  Data  Editor.”  American Economic Association  [publisher],https://doi.org/10.3886/E135023V1


## External dependencies

A data source has internal files, not otherwise made available. 

The data source is:

> Vilhuber,  Lars. 2021a.  “Process  data  for  the AEA  Pre-publication  Verification  Service.” American Economic Association [publisher], https://doi.org/10.3886/E117876V2

## Running code

See the `programs/README.md` file for further details.

Two additional utility scripts are present, for packaging up the contents of the git repository. They are not needed to either create figures and tables, or to compile the article.

## Structure

### Data

Some data needs to be downloaded before being able to run programs; other data is provided within this repository:

#### List of Lab members

```
data/replicationlab_members.txt
```

Listed in the article appendix.

#### Data for pre-production verification

See `data/jira/anon/README.md` for more details. 
Source: Vilhuber (2020a)

```
data/jira/anon/jira.anon.RDS
data/jira/anon/jira.anon.csv
data/jira/anon/README.md
```


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

## Technical notes (apply only to the Github version)

The repository references at least one external git repo. So do

```
git submodule update --init --recursive
```

after cloning it.

