## check and install packages.

## check if environment manager is needed, if yes install
if (!require("renv")) install.packages("renv")
if (!require("pacman")) install.packages("pacman")

#### AUTOMATIC INSTALLATION -----------------------------------------------------------------------

## find package dependencies in the project
used_packages <- unique(renv::dependencies()$Package)

## load or install packages
pacman::p_load(char = used_packages)

#### Manual installation --------------------------------------------------------------------------

# if (!require("pacman")) install.packages("pacman")

# ## packages are sorted by domain. if installation is not working
# ## smoothly you can uncomment individual packages that may not be necessary for the current analysis 
# 
# packages = c(
#   
#   ## utils
#   "here",
#   "jtools",
#   
#   ## data wrangling
#   "tidyverse",
#   "broom",
#   # 
#   # ## plotting
#   # "gghalves",
#   # "ggthemes",
#   # "RColorBrewer",
#   # "scales",
#   # "showtext",
#   # "grid",
#   # "patchwork",
#   # "ggdist",
#   # "cowplot",
#   # "sjPlot",
#   # "ggstance",
#   # "ggsignif",
#   
#   ## stats
#   "lme4",
#   "lmerTest",
#   "kableExtra",
#   
#   ## bayesian analyses 
#   "rstan",
#   "rstanarm",
#   "brms",
#   "bayesplot",
#   "repmod",
#   "posterior",
#   "tidybayes"
# )

