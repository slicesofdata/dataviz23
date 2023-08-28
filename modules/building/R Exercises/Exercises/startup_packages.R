# Update all current packages to newest versions
# update.packages(repos = "http://cran.rstudio.com")

# List of useful packages
pkg <- c("xlsx", "car", "lsr", "doBy", "ez", "foreign", "Hmisc", "magrittr", 
         "lattice", "latticeExtra", "multcomp", "pastecs", "reshape2", 
         "stats", "swirl", "lawstat", "plyr", "RCurl", "readxl", "languageR",
         "nortest", "psych", "moments", "lmtest", "tidyr", "dplyr", "ggplot2", 
         "knitr", "rmarkdown", "rdrop2", "ggvis", "plotrix", "cowplot", "libcurl",
         "DiagrammeR", "foreach", "devtools", "readr2", "ezsummary", "RGoogleDocs"
         "asbio")

# Check if packages are not installed and assign the
# names of the uninstalled packages to the variable new.pkg
new.pkg <- pkg[!(pkg %in% installed.packages())]

# If there are any packages in the list that aren't installed,
# install them
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}

# And some other packages
bayespkg <- c("BRugs", "BayesFactor", "rstanarm", "Rtools", 
              "glmmBUGS", "brms", "Xcode")
new.pkg <- bayespkg[!(bayespkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")
}
