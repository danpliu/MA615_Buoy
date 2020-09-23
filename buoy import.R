###For the format of data varies from year to year, the 30 collections need to be separated.

library(tidyverse)
library(stringr)

### make URLs
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1987:2016)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

###  Read the data from the website
##Initial Y1987-Y1999 
for (i in 1:13){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  if(i == 1){MR <- file}
  else{MR <- rbind.data.frame(MR, file)}
}

##Y2000-Y2004 added a variable of 'TIDE'
for (i in 14:18){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )
  file <- get(filenames[i])
  #Y2000-Y2004 added a variable of 'TIDE'
  file=file[,-16]
  colnames(file)[1] <-"YYYY"
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  MR <- rbind.data.frame(MR, file)
}

##Y2005-Y2006 added a variable of 'mm'
for (i in 19:20){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )
  file <- get(filenames[i])
  #Y2005-Y2006 added a variable of 'mm' 
  file=file[,-17]
  file=file[,-5]
  colnames(file)[1] <-"YYYY"
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  MR <- rbind.data.frame(MR, file)
}

## The combined variable of 'WSPD GST' separated into 'WSPD' and 'GSP'
z=separate(MR,col='WSPD GST',into=c('WSPD','GSP','1','2'),sep='[ ]')
zGSP=z$GSP==''
z$GSP[zGSP]=z$'1'[zGSP]
zGSP=z$GSP==''
z$GSP[zGSP]=z$'2'[zGSP]
z=z[,-9]
z=z[,-8]
##Y2007 the variable of 'WD', 'WSPD' and 'GST' combined as 'WDIR WSPD GST'
##(Optional) Y2007 the variable of 'MWD' and 'BAR' combined as 'MWD PRES'
for (i in 21:30){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )
    
  file <- get(filenames[i])
  #Y2007 added a variable 'WDIR' in 'WDIR WSPD GST'
  #(Optional) Y2007 the variable of 'MWD' and 'BAR' combined as 'MWD PRES'
  x=separate(file,col='WDIR WSPD GST',into=c('WDIR','WSPD','GSP','1','2'),sep='[ ]')
  xWSP=x$WSPD==''
  x$WSPD[xWSP]=x$GSP[xWSP]
  x$GSP[xWSP]=x$'1'[xWSP]
  xGSP=x$GSP==''
  if (colnames(x)[15]=='MWD'){colnames(x)[15]='BAR'}
  else{x=separate(x,col='MWD   PRES',into=c('MWD','BAR'),sep='[ ]')}
  x$GSP[xGSP]=x$'2'[xGSP]
  x=x[,-20]
  x=x[,-10]
  x=x[,-9]
  x=x[,-5]
  colnames(x)[5]='WD'
  x=x[-1,]
  file=x
  colnames(file)[1] <-"YYYY"
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  MR <- rbind.data.frame(MR, file)
}

##Unified the time format
MR$YYYY=as.numeric(MR$YYYY)
MR$YYYY[MR$YYYY<1000]=MR$YYYY[MR$YYYY<1000]+1900

###Save data
save(MR,file='MR.Rdata')