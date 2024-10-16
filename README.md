README
================


# Beforerunning code

The repository is organized as a RStudio Project folder. This makes it easier to manage packages and relative paths. (see https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects for more info). It also means that to run the code as is, you should do it
in RStudio, and load the project from the file "GSMB.RProj" in the main folder.

If you don't have RStudio installed, you will have to change the paths to match those on your local machine.

# How to interact with this repository

Important files:

analyses_experimental_data.qmd and simulations.qmd are Quarto files that you can use to run the main analyses of the paper.
If you are not familiar with Quarto, the code is still executable in the console or can be copy pasted into a simple R script. 

General content of each folder:

- data_experiments the full dataset for all the experiments
- data_simulations contains the output of the simulations that is then analysed in simulations.qmd. Simulations take some time to run so 
it's conveniente to just load this file.

- scripts: scripts used to analyse the data.

- tables: demographics information, summary of model fitting output.

- modelfits: output files of the regression models. 

At the moment, this repository contains all the code necessary to replicate the analyses in the manuscript and supplement.
In the future, I plan to add more script to easily run only individual analyses.

For any questions, you can reach out to: andrea.gradassi@gmail.com.