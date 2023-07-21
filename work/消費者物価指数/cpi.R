install.packages("openxlsx")

# library(tidyverse)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr)

cpi_data__ <- read.xlsx("project/消費者物価指数/aq06-1.xlsx")

cpi_data_ <- cpi_data__[13:nrow(cpi_data__)-2, 2:11]
head(cpi_data_)
colnames(cpi_data_) <- c("term_short", "junk1", "junk2", "index", "term", "総合", "生鮮食品を除く総合", "持家の帰属家賃を除く総合", "生鮮食品及びエネルギーを除く総合", "食料（酒類を除く）及びエネルギーを除く総合")
head(cpi_data_)
cpi_data <- select(.data=cpi_data_, -c("junk1", "junk2"))
cpi_data <- select(.data=cpi_data, c("index", "term_short", everything()))
head(cpi_data, 3)
tail(cpi_data, 3)
# View(cpi_data)
str(cpi_data)
nrow(cpi_data)
length(cpi_data)

glimpse(cpi_data)
cpi_data$index <- as.integer(cpi_data$index)
cpi_data$総合 <- as.numeric(cpi_data$総合)
cpi_data$生鮮食品を除く総合 <- as.numeric(cpi_data$生鮮食品を除く総合)
cpi_data$持家の帰属家賃を除く総合 <- as.numeric(cpi_data$持家の帰属家賃を除く総合)
cpi_data$生鮮食品及びエネルギーを除く総合 <- as.numeric(cpi_data$生鮮食品及びエネルギーを除く総合)
cpi_data$`食料（酒類を除く）及びエネルギーを除く総合` <- as.numeric(cpi_data$`食料（酒類を除く）及びエネルギーを除く総合`)
glimpse(cpi_data)

cpi_data <- cpi_data[(nrow(cpi_data)-21):nrow(cpi_data),]

x_labs <- trimws(cpi_data$term_short)
x_index <- cpi_data$index

g <- ggplot(data=cpi_data, aes(x=index))
# g <- g + geom_point(alpha=0.3)
g <- g + geom_line(aes(y=総合, colour="総合"), linewidth = 2)
g <- g + geom_line(aes(y=生鮮食品を除く総合, colour="生鮮食品を除く総合"))
g <- g + geom_line(aes(y=持家の帰属家賃を除く総合, colour="持家の帰属家賃を除く総合"))
g <- g + geom_line(aes(y=生鮮食品及びエネルギーを除く総合, colour="生鮮食品及びエネルギーを除く総合"))
g <- g + geom_line(aes(y=`食料（酒類を除く）及びエネルギーを除く総合`, colour="食料（酒類を除く）及びエネルギーを除く総合"))
# g <- g + scale_x_reverse()
g <- g + scale_x_discrete("期間", limits=x_index, labels=x_labs)
g <- g + theme(axis.text.x = element_text(angle=70, hjust=1),
               legend.position = c(0.38, 0.95),
               legend.justification = c(1, 1))
plot(g)
