# Expressive/Deadpan Project
## Overview
This paper contains the data, code, and figures from Anderson, Ling, & Schutz (under review). To use this code, you must first install [*R* and *RStudio*](https://rstudio-education.github.io/hopr/starting.html), along with [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
## File organization
Analysis code is organized in .qmd and .r files within the repository. 
- Data are contained in the `/data` folder.  
- Custom functions are viewable in the `/lib` folder. 
- Figures from the analyses are viewable in the `/img` folder.
- Analysis scripts are located in the `/scripts` folder. 
# Getting started
Analyses can be replicated using the `renv` package to restore dependencies and versions from the original analysis in an isolated project folder. 
Before this, you must have git installed and configured, as well as *R*. I also recommend installing *RStudio* as it will allow you to open .Rproj files.
## Cloning repository
In a terminal, navigate to the location you would like to write the project files to. Then, clone the repository: 
`git clone https://github.com/cmndrsn/expressive-deadpan` (run from a terminal)
## Reproducing analyses with renv (recommended)
- Ensure the package `renv` is installed for *R*.
  - Inside *R*, run the command `install.packages("renv")`.
  - Once installed, open the project file `expressive-vs-deadpan.Rproj` and run `renv::restore()` in the console. This will load the required dependencies to reproduce analyses.
## Running analysis script
- Once all dependencies are installed, open `expressive-vs-deadpan.Rproj` in RStudio.
- From the file navigator:
  - open `scripts/va-comparison.qmd` from within the project to reproduce analyses of emotion ratings
  - open `scripts/va-modeling.R` from within the project to reproduce regression analyses


