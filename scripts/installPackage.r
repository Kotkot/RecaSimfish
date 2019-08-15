### Installation code for R package LiceSupport
#############################################
dir<-"/nr/project/stat/RammeavtaleHI_17_20/SimuleringIngunn/RpackageSimFishery"## Directory where the package code is stored

###install.packages("MASS")  ## must be installed if not already installed
library(MASS)

origwd<-getwd()
setwd(dir)
devtools::document()
###devtools::check(manual=TRUE)
devtools::build(manual=TRUE)
devtools::install(dir, source = TRUE,args="--no-lock")
setwd(origwd)

pack <- "RpackageSimFishery"
path <- find.package(pack)
system(paste("rm ",pack,".pdf",sep=""))
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
