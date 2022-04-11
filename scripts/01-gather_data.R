library(tesseract)
library(tidyverse)
library(reader)
text <- tesseract::ocr(image="input.png", engine = tesseract("eng"))

sink("outfile.txt")
cat(text)
sink()

d <- readLines("outfile.txt")

raw_data <- read.table(text = d,sep = ' ',fill = TRUE)

write.csv(raw_data,"raw_data.csv", row.names = FALSE)
