# HC-One_Recruitment_Group10

Welcome to Group 10 Project!

This files contains the guidelines that are required to successfully load the project.

Step 1: Install packages

reshape2, plyr, tidyverse, stringr, lubridate, dplyr, openxlsx, writexl, ggplot2, viridis, purrr, tidyselect, ggpubr,digest, tibble, DataExplorer, nortest

Step 2:working directory

To load the project, you'll first need to `setwd()` into the directory
where this README file is located. Then you need to run the following two
lines of R code:

	library('ProjectTemplate')
	load.project()
	
After you enter the second line of code, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

Step 3: Run the code

Once that's done, you can execute any code you'd like. All the analysis code is contained in the `src` directory.
and all the preprocessing code is in the 'munge' directory

step 4:
The power BI can be found in the root directory of this file.



