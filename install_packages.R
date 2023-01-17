# install required packages
pack <- c("knitr", "revealjs", "tidyverse", "plotrix", "magrittr", "ggplot2", "lme4", "merTools", "spaMM", "glmmTMB", "DHARMa", "car")

install.packages(pack[!pack %in% row.names(installed.packages())])

# to avoid any problems, let's all have the latest version of the packages
update.packages(ask = FALSE)
