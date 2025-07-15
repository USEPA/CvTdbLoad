# cvtdbLoad

# Background
The Concentration versus Time Database (CvTdb) contains manually curated time-series data and associated metadata for *in vivo* toxicokinetic ("TK") studies on organic chemicals available in the scientific literature. The database has been developed in close coordination with leading researchers at the EPA with specialties relating to toxicology and toxicokinetic modeling. These data inform chemical safety analysis and allow evaluation of the relationship between administered doses and internal concentrations of a substance. These data can also be used to build or evaluate physiologically based pharmacokinetic (PBPK) and physiologically-based (PBTK) models (such as the [*httk*](https://cran.r-project.org/web/packages/httk/index.html) R package), which simulate the absorption, distribution, metabolism, and elimination of a chemical. The database also contains toxicokinetic parameters, including volume of distribution and elimination half-life, which are calculated across all data associated with a particular compound using the publicly available pharmacokinetic curve-fitting software [*invivoPKfit*](https://CRAN.R-project.org/package=invivoPKfit). This version 2.0.0 release builds upon the original [v1.0.0](https://github.com/USEPA/CompTox-PK-CvTdb/releases/tag/v1.0.0) “legacy” database released with Sayre, Wambaugh, and Grulke (2020) and the minor [v1.1.0](https://github.com/USEPA/CompTox-PK-CvTdb/releases/tag/v1.1.0) database release from 2021 that added the Showa Pharmaceutical University dataset. The code, documentation, and vignettes associated with the database release are available on GitHub ([CompTox-PK-CvTdb](https://github.com/USEPA/CompTox-PK-CvTdb); [CvTdbLoad](https://github.com/USEPA/cvtdbload)). The database is also available for download through the public CCTE EPA [Clowder repository] (**link TBD**) (no user account required).

# Repository Content
This repository contains the R scripts used to process templates into CvTdb, perform term and unit normalizations/conversions, and generate QA reports and templates for database review and refinement.
>Note, the code provided is for publication transparency sake only. Users are not meant to run the code and reproduce the database themselves. The generation of the database is a semi-automated process, so it is not possible to completely reproduce the database from running the provided code alone. Please access the complete database data from the links provided below.

# Where to access ToxValDB data
- US EPA FigShare Dataset (**link TBD**)
	- Versioned releases of CvTdb in SQLite file format with associated documentation
	- Note, the FigShare DOI link will land on the most recent version of the FigShare posting. Use the version dropdown menu to navigate to the desired release version based on the dataset title.
	- Version v2.0.0 was also released as a Zenodo Dataset (**link TBD**)
- [CompTox-PK-CvTdb](https://github.com/USEPA/CompTox-PK-CvTdb)
	 - Download the latest version SQLite file from the latest release.

# Repository Links
- [cvtdbLoad](https://github.com/USEPA/cvtdbload)
- [CompTox-PK-CvTdb](https://github.com/USEPA/CompTox-PK-CvTdb)

# Contribute to CvTdb
See [CompTox-PK-CvTdb](https://github.com/USEPA/CompTox-PK-CvTdb) for more information on how to contribute to CvTdb.

# Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
