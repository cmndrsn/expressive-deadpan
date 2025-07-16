# Expressive/Deadpan Project
## Overview
This paper contains the data, code, and figures from Anderson, Ling, & Schutz (under review). We performed analyses using *R* (version 4.2.3) on the Windows 10 operating system (version 22H2). To use this code, you must first install [*R* and *RStudio*](https://rstudio-education.github.io/hopr/starting.html), along with [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
## File organization
Analysis code is organized in .qmd and .r files within the repository. 
- Data are contained in the `/Data` folder.  
- *R* dependencies and custom functions are viewable in the `/R` folder. It contains the following files:
  - `Libraries.R`. Module containing all dependencies required for the analysis code to run.
  - `Functions.R` Module containing custom functions written for this project.
  - `Preprocessing.R` Script including additional code for figure and data formatting.
- Figures from the analyses are viewable in the `/Figures` folder.
- `Analysis/Analysis.qmd` contains the analyses from the manuscript. 
# Getting started
Analyses can be replicated in one of two ways: one involves using the `renv` package to restore dependencies and versions from the original analysis in an isolated project folder. The second involves manually installing packages. I recommend the first method, as it reduces the chance of package conflicts affecting analysis reproducibility.
Before this, you must have git installed and configured, as well as *R*. I also recommend installing *RStudio* as it will allow you to open .Rproj files.
## Cloning repository
In a terminal, navigate to the location you would like to write the project files to. Then, clone the repository by running the following line of code: 
`git install https://github.com/cmndrsn/expressive-deadpan-project`  
## Reproducing analyses with renv (recommended)
- Ensure the package `renv` is installed for *R*.
  - Inside *R*, run the command `install.packages("renv")`.
  - Once installed, open the project file `Expressive-Deadpan.Rproj` and run `renv::restore()` in the console. This will load the required dependencies to reproduce analyses.
## Reproducing analyses after manually installing packages (not recommended)
- To get started, open `/R/Libraries.R` and ensure all libraries are installed. 
  - The *R* command to install a package is `install.packages()`. For example, to install the dplyr package, run the following line of code:
 `install.packages('dplyr')`
  - The success of reproducing analyses may depend on specific package versions. If you encounter issues through this method, you can see the versions used to produce the analyses in the `renv.lock` file. This process will likely be quite tedious and time-consuming, so I recommend using `renv` instead. 
## Running analysis script
- Once all dependencies are installed, open `Expressive-Deadpan.Rproj` in RStudio. From the file navigator, open `Analysis/Analysis.qmd` from within the RStudio to reproduce analyses. 
