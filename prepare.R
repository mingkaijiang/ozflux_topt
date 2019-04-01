#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Create output folder
if(!dir.exists("output/processed_data")) {
    dir.create("output/processed_data", showWarnings = FALSE)
}

#### Create output folder
if(!dir.exists("output/plots")) {
    dir.create("output/plots", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               grid,
               cowplot,
               ncdf4,
               quantreg,
               zoo,
               gam,
               Metrics,
               mgcv,
               e1071,
               xts,
               lubridate,
               nnet,
               sm,
               gridExtra)  


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

