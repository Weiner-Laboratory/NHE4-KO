# Convert .rds and .qs files to .qs2 format
# this is supposed to have better long-term stability than the .qs format.
# load libraries ----
library(Seurat)
library(qs) # used for faster Seurat data save and read access
library(qs2)
library(tools)
# load idw functions ----
source("/blue/weineid/version-controlled-software/functions/get input.R")
source("/blue/weineid/version-controlled-software/functions/data set information.R")
source("/blue/weineid/version-controlled-software/functions/read data file.R")
data.info <- get.data.info()
# load data set ----
for (file in data.info$filename) {
  data.file.name <- paste0("/blue/weineid/datasets/", file)
  file.extension <- file_ext(data.file.name)
  message("Assessing ", file, " with extension ", file.extension)
  qs2.file.name <- paste0(data.file.name, ".qs2")
  if (file.exists(qs2.file.name)) {
    message (qs2.file.name, " already exists ...")
  } else {
    message("Reading data file-> ", data.file.name)
    data.set <- read.data.file(data.file.name)
    message ("   Saving new file with name, ", qs2.file.name)
    qs_save(data.set, file = qs2.file.name)
  }
}
message("Conversion completed")