source('munge.r')
library(lattice)
library(stringr)
library(latticeExtra)

shinyServer(function(input, output){
  downloads <<- list()                   #Global, caches parsed telemetry by site
  bound <<- NULL                         #Global, caches merged telemetries
  sites <- c(
    'Hurricane Ridge'='http://www.turns-all-year.com/woptelemetry.php?id=hurricaneridge',
    'Mount Baker'='http://www.turns-all-year.com/woptelemetry.php?id=mtbakerskiarea',
    'Washington Pass'='http://www.turns-all-year.com/woptelemetry.php?id=washingtonpass',
    'Mazama'='http://www.turns-all-year.com/woptelemetry.php?id=mazama',
    'Mission Ridge'='http://www.turns-all-year.com/woptelemetry.php?id=missionridge',
    'Stevens 2'='http://www.turns-all-year.com/woptelemetry.php?id=stevenshwy2',
    'Stevens Ski'='http://www.turns-all-year.com/woptelemetry.php?id=stevensskiarea',
    'Snoqualmie'='http://www.turns-all-year.com/woptelemetry.php?id=snoqualmiepass',
    'Alpental'='http://www.turns-all-year.com/woptelemetry.php?id=alpental'
    )
  
  fieldTypes <- c('Wind.Max'='MPH', 'Wind.Avg'='MPH', 'Temp'='TEMP', 'RH'='RH', 'Total.Snow'='SNOW_DEPTH', 'Hour.Prec'='HOUR', 'Hr24.Snow'='DAY', 'Wind.Dir'='DIR')

  ## As a side-effect
datasetCache <- reactive(function() {
  bound <<- NULL
    if(!is.null(input$dataset)){
      for(d in input$dataset){
        if(is.null(downloads[[d]])){
          downloads[[d]] <- mungeNWAC(readLines(sites[[d]], warn=FALSE), smooth=TRUE)
          message('download triggered')
        }
         if(is.null(bound)) {
          bound <- downloads[[d]]$tab          
        }
        else{
          bound <- merge(bound, downloads[[d]]$tab, by=c('Date', 'year', 'month', 'day', 'hour'))
        }
      }
    }
    bound <<- bound
    downloads <<- downloads
    downloads
  })

  
output$datasetControls <- reactiveUI(function() {
  dl <- datasetCache()
  choices <- NULL
  for(d in input$dataset){
    choices <- c(choices, prettyifynames(names(dl[[d]]$tab)))
  }
  checkboxGroupInput('series', "Choose series", choices)
})

## output$datasetControls <- reactiveUI(function() {
##   dl <- datasetCache()
##   tl <- list()
##   if(!is.null(input$dataset)){
##     d <- input$dataset[1]
##     return(checkboxGroupInput('fixed', sprintf("Choose %s series", d), choices=prettyifynames(names(dl[[d]]$tab))))
##   }
##   else{
##     return(NULL)
##   }
## })


  testFields <- function(fields){
 if(length(input$series)>0){
      plotTypes <- unlist(lapply(input$series, function(x){fieldTypes[str_detect(x, names(fieldTypes))]}))
      return(any(fields %in% plotTypes))
    }
    return(FALSE)
  }
  
  output$hastemp_precip <- reactive(function(){testFields(c('TEMP', 'HOUR'))})
  output$hassnowbase <- reactive(function(){testFields(c('SNOW_DEPTH', 'DAY'))})
  output$haswind <- reactive(function(){testFields(c('DIR', 'MPH'))})

  
  
  makeDoubleChart <- function(fields, lattice1, lattice2){
    ## fields: what series to plot.  Must supply two and only two
    ## lattice1: first lattice call, must return un-plotted object
    ## lattice2: IBID.
    stopifnot(length(fields)==2)
 if(length(input$series)>0){
      plotTypes <- unlist(lapply(input$series, function(x){fieldTypes[str_detect(x, names(fieldTypes))]}))
      tempF <- which(plotTypes == fields[1])
      hourF <- which(plotTypes == fields[2])
      form1 <- form2 <- NULL
      if(length(tempF)>0){
        form1 <- sprintf('%s ~ Date', paste(input$series[tempF], collapse='+'))
        p1 <- lattice1(as.formula(form1))
      }
      if(length(hourF)>0){
        form2 <- sprintf('%s ~ Date', paste(input$series[hourF], collapse='+'))
        p2 <- lattice2(as.formula(form2))
      }
      if(!is.null(form1) && !is.null(form2)){
      print(p1 + as.layer(p2, y.same=FALSE, x.same=TRUE))
    } else if(!is.null(form1)){
      print(p1)
    } else{
      print(p2)
    }
    }
    return(NULL)
}

  output$plot_temp <- reactivePlot(function(){
       makeDoubleChart(c('TEMP', 'HOUR'),
                       lattice1=function(f){xyplot(f, bound, type='l') + layer(panel.abline(h=32, col='grey'))},
                       lattice2=function(f){xyplot(as.formula(f), bound, type='h')})})


  output$plot_wind <- reactivePlot(function(){
       makeDoubleChart(c('MPH', 'DIR'),
                       lattice1=function(f){xyplot(f, bound, type='l') + layer(panel.abline(h=20, col='red'))},
                       lattice2=function(f){xyplot(as.formula(f), bound)})})
       
   output$plot_snow <- reactivePlot(function(){
       makeDoubleChart(c('DAY', 'SNOW_DEPTH'),
                       lattice1=function(f){xyplot(f, bound, type='b') + layer(panel.abline(h=12, col='red'))},
                       lattice2=function(f){xyplot(as.formula(f), bound, type='l', ylim=c(0, 140))})})
       
  
       
 
  ## output$plottype <- reactive(function(){
  ##   sel <- input$dataset
  ##   bound <- NULL
  ##   plotWindows <- series <- list()
  ##   if(!is.null(sel)){
  ##     for(d in sel){
  ##       if(is.null(bound)) {
  ##         bound <- downloads[[d]]$tab          
  ##       }
  ##       else{
  ##         bound <- merge(bound, downloads[[d]]$tab, by=c('Date', 'year', 'month', 'day', 'hour'))
  ##       }
  ##         #series[[d]] <- input[[d]]
  ##         #whichind <- match(series[[d]], names(downloads[[d]]))
  ##         #plotWindows[[d]] <- downloads[[d]]$plotTypes[whichind]
  ##     }

  ##   }
  ## })
  
 output$debug <- reactivePrint(function(){print(input$dataset)
                                         lapply(input$dataset, function(x){print(input[[x]])})
                                              })

})
