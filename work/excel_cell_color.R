
library(xlsx)

setwd("C:/Users/liang/Desktop/")

dt = loadWorkbook('Bt_nonBT.xlsx')


library(readxl)
sht = excel_sheets('Bt_nonBT.xlsx')
sht


cellColor <- function(style) 
{
  fg  <- style$getFillForegroundXSSFColor()
  rgb <- tryCatch(fg$getRgb(), error = function(e) NULL)
  rgb <- paste(rgb, collapse = "")
  return(rgb)
}



cols = c('green', 'yellow', 'non')
codes = c('70ad47','ffff00', 'ed7d31')

for (i in 1:9){
  
  st1 = getSheets(dt)[[i]]
  rows = getRows(st1)
  cells = getCells(rows)
  
  styles <- sapply(cells, getCellStyle) #This will get the styles
  cols = sapply(styles, cellColor)
  
  
  col_df = data.frame(matrix(cols,nrow=30,ncol=31,byrow=T))
  col_df[col_df=='70ad47'] = 1
  col_df[col_df=='92d050'] = 1
  col_df[col_df=='ffff00'] = 0
  col_df[col_df=='ed7d31'] = NA
  
  col_df2 = col_df[ , colSums(is.na(col_df)) == 0]
  nm = paste("ID_",sht[i],".csv",sep="")
  
  write.csv(col_df2,nm,row.names = F)
  
}
