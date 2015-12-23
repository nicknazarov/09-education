


#dat <- download.file("https://d396qusza40orc.cloudfront.net/", destfile ="./getdata-data-ss06hid.csv", mode = "curl" ) 


dat<- read.csv("/home/nazarov/Загрузки/getdata-data-ss06hid.csv")
#temp<-dat [which (dat$VAL==24),]

library(XLConnect)

dat<- readWorksheet(loadWorkbook("/home/nazarov/Загрузки/getdata-data-DATA.gov_NGAP.xlsx"), sheet=1, startRow =18, endRow=23 ,startCol=7,endCol=15)
sum(dat$Zip*dat$Ext,na.rm=T) 
install.packages("XML")




library(XLM)
#source(tmp/RtmpPKPtn0/downloaded_packages)
doc <- xmlTreeParse("/home/nazarov/Загрузки/getdata-data-restaurants.xml",useInternal=TRUE )
