library(stringr)
library(chron)
typesTop<-c('Temp', 'RH', 'Wind', 'Wind', 'Wind', 'Solar', 'Press')
modifiersTop<-c('F', NA, 'Avg', 'Max', 'Dir.', NA, NA)
typesBot<-c('Snow', 'Snow', 'Prec.', 'Prec.')
modifiersBot<-c('24Hr', 'Total', 'Hour', 'Total')



mungeNWAC <- function(ss, smooth=FALSE){
  ## ss is a list of lines from t-a-y 10 day telemetry
  ## smooth: try to fix bad values for snow depth measurements
tt<-try({
fieldLine <- which(str_detect(ss, 'MM/DD'))[1]
})
if(class(tt)=='try-error') stop("Couldn't parse telemetry, sorry")
ss <- ss[-seq_len(fieldLine-1)]
headerLines<-ss[1:3]
ss<-ss[-1:-3]
headerLines[3]<-str_replace_all(headerLines[3], '&#39;', '')
headerLines[1]<-str_replace_all(headerLines[1], 'MM/DD', 'MM DD')
headerLines[1]<-str_replace_all(headerLines[1], '24 Hr', 'Hr24')
substr(headerLines[2], 1, 7)<- ' .   . '
substr(headerLines[3], 1, 10)<- ' .   .   . '
htab<-read.table(textConnection(headerLines), stringsAsFactors=F)

# hasTT <- match(htab[1,], typesTop)
# modTT <- match(htab[2,!is.na(hasTT)], modifiersTop)
NM<-lapply(htab, function(x){paste(x, collapse='.')})
NM[1:3]<-c('month', "day", 'hour')
NM <- str_replace_all(as.character(unlist(NM)), '[.]+', '.')
otherjunk <- str_detect(ss, '^\\s*[^123456789[:blank:]]+')
ss <- ss[!otherjunk]
morejunk <- str_detect(ss, '&#')
ss<-ss[!morejunk]
## Try to detect if a field is missing and pad with NA
ss <- str_replace_all(ss, '([0-9]+)\\s{8,}', '\\1  NA  ')

tab <- read.table(textConnection(ss), header=F, strip.white=TRUE,stringsAsFactors=TRUE, col.names=NM, fill=TRUE)
thisyear<-as.numeric(as.character(years(Sys.Date())))
if(all(c(1, 12) %in% tab$month)){ #worry about new year
  tab$year<- ifelse(tab$month==12, thisyear-1, thisyear)
} else{
  tab$year<-thisyear
}
dateStr<-with(tab, paste(month, day, year, sep='/'))
timeStr<-with(tab, paste(hour/100, '00', '00', sep=':'))
tab$Date<-chron(dateStr, timeStr, format=c(dates='m/d/y', times='h:m:s'))

list(tab=tab, header=headerLines)
}

kill <- c('month', 'day', 'hour', 'year', 'Date')
prettyifynames <- function(nwacNames){
  nw <- nwacNames[!(nwacNames %in% kill)]
}

## #for example
## xyplot(Temp.F.4850+Temp.F.3950~Date, tab, type='l', panel=function(...){panel.xyplot(...); panel.abline(h=32, col='grey')}) +
##   as.layer(xyplot(Hour.Prec..3950 ~ Date, tab, type='h', col=2), y.same=F)
