# UNCOMMENT FOR TESTING

# library(gtalibrary)
# library(xlsx)
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
# load("17 Shiny/4 data kitchen/log/kitchen log.Rdata")
# kl = kitchen.log[550,]

# PROCESSING

tryCatch({

if(as.character(kl$coverage.period)!=""){
  c.period=as.numeric(paste(unlist(strsplit(as.character(kl$coverage.period),","))))}else{c.period=NULL}

if(as.character(kl$gta.evaluation)!=""){
  gta.eval=paste(unlist(strsplit(as.character(kl$gta.evaluation),",")))}else{gta.eval=NULL}

if(as.character(kl$affected.flows)!=""){
  a.flow=paste(unlist(strsplit(as.character(kl$affected.flows),",")))}else{a.flow=NULL}

if(as.character(kl$importers)!=""){
  imps=paste(unlist(strsplit(as.character(kl$importers),",")))
  if("" %in% imps){imps = imps[!imps==""]}}else{imps=NULL}

if(as.character(kl$nr.also.importers)!="NA"){
  nr.a.i=paste(unlist(strsplit(as.character(kl$nr.also.importers),",")))}else{nr.a.i=NULL}

if(as.character(kl$exporters)!=""){
  exps=paste(unlist(strsplit(as.character(kl$exporters),",")))
  if("" %in% exps){exps = exps[!exps==""]}}else{exps=NULL}

if(as.character(kl$nr.also.exporters)!="NA"){
  nr.a.e=paste(unlist(strsplit(as.character(kl$nr.also.exporters),",")))}else{nr.a.e=NULL}

if(as.character(kl$implementers)!=""){
  ij=paste(unlist(strsplit(as.character(kl$implementers),",")))
  ij <- ij[ij != ""]}else{ij=NULL}

if(as.character(kl$implementer.role)!="NA,NA"){
  ij.role=paste(unlist(strsplit(as.character(kl$implementer.role),",")))}else{ij.role=NULL}

if(as.character(kl$announcement.period)!="NA,NA"){
  a.period=paste(unlist(strsplit(as.character(kl$announcement.period),",")))
  a.period[a.period=="NA"] <- NA
}else{a.period=NULL}

if(as.character(kl$implementation.period)!="NA,NA"){
  i.period=paste(unlist(strsplit(as.character(kl$implementation.period),",")))
  i.period[i.period=="NA"] <- NA
}else{i.period=NULL}

if(as.character(kl$revocation.period)!="NA,NA"){
  r.period=paste(unlist(strsplit(as.character(kl$revocation.period),",")))
  r.period[r.period=="NA"] <- NA
}else{r.period=NULL}

if(as.character(kl$in.force.today)!=""){
  ift=paste(unlist(strsplit(as.character(kl$in.force.today),",")))}else{ift=NULL}

if(as.character(kl$intervention.types)!=""){
  i.types=paste(unlist(strsplit(as.character(kl$intervention.types),",")))
  nes.list <- c("Import-related non-tariff measure","Export-related non-tariff measure","Public procurement","State aid","FDI: Treatment and operations")
  if("" %in% i.types){i.types = i.types[!i.types==""]}
  if (any(i.types %in% nes.list)){
    for (i in 1:length(i.types)){
      if (i.types[i] %in% nes.list){i.types[i] <- paste0(i.types[i],", nes")}
      if (i.types[i] ==" nes"){i.types[i] <- 0}}
    i.types <- i.types[i.types!=0]
  }}else{i.types=NULL}

if(as.character(kl$mast.chapters)!=""){
  mast=paste(unlist(strsplit(as.character(kl$mast.chapters),",")))
  if("" %in% mast){mast = mast[!mast==""]}}else{mast=NULL}

if(! grepl("any", as.character(kl$implementation.level))){
  il=paste(unlist(strsplit(as.character(kl$implementation.level),",")))}else{il=NULL}

if(! grepl("any", as.character(kl$eligible.firms))){
  ef=paste(unlist(strsplit(as.character(kl$eligible.firms),",")))
  if("" %in% ef){ef = ef[!ef==""]}}else{ef=NULL}

if(as.character(kl$cpc.sectors)!=""){
  cpc=as.numeric(paste(unlist(strsplit(as.character(kl$cpc.sectors),","))))
  if("" %in% cpc){cpc = cpc[!cpc==""]}}else{cpc=NULL}

if(as.character(kl$hs.codes)!=""){
  hs=as.numeric(paste(unlist(strsplit(as.character(kl$hs.codes),","))))
  if("" %in% hs){hs = hs[!hs==""]}}else{hs=NULL}

if(as.character(kl$intervention.ids)!=""){
  int.id=paste(unlist(strsplit(as.character(kl$intervention.ids),",")))}else{int.id=NULL}

if(as.character(kl$lag.adjustment)!=""){
  lag=paste(paste(unlist(strsplit(as.character(kl$lag.adjustment),"-")))[2],
            paste(unlist(strsplit(as.character(kl$lag.adjustment),"-")))[3],sep="-")}else{lag=NULL}


if(as.character(kl$choose.output)!=""){
  trade.statistic=as.character(kl$choose.output)}else{trade.statistic="share"}

if(as.character(kl$choose.tradebase)!=""){
  trade.data=as.character(kl$choose.tradebase)}else{trade.data="base"}

if(as.character(kl$hit.brackets)!=""){
  hb=paste(unlist(strsplit(as.character(kl$hit.brackets),",")))}else{hb=c(1,99999)}



## running the function
gta_trade_coverage(
  data.path="data/master_plus.Rdata",
  replica.path="data/database replica/database replica - parts - base.Rdata",
  coverage.period=c.period,
  current.year.todate=kl$current.year.todate==T,
  gta.evaluation= gta.eval,
  affected.flows = a.flow,
  importers = imps,
  group.importers = kl$group.importers==T,
  keep.importers = kl$keep.importers==T,
  separate.importer.groups = kl$separate.importer.groups==T,
  nr.also.importers = nr.a.i,
  jointly.affected.importers = kl$jointly.affected.importers == T,
  exporters = exps,
  group.exporters = kl$group.exporters==T,
  keep.exporters = kl$keep.exporters==T,
  separate.exporter.groups = kl$separate.exporter.groups==T,
  nr.also.exporters = nr.a.e,
  jointly.affected.exporters = kl$jointly.affected.exporters == T,
  implementers = ij,
  implementer.role = ij.role,
  keep.implementer = kl$keep.implementer==T,
  announcement.period = a.period,
  implementation.period = i.period,
  revocation.period = r.period,
  in.force.today = ift,
  intervention.types = i.types,
  keep.type = kl$keep.type==T,
  group.type=kl$group.type==T,
  mast.chapters = mast,
  keep.mast = kl$keep.mast==T,
  group.mast=kl$group.mast==T,
  implementation.level = il,
  keep.level = kl$keep.level==T,
  eligible.firms = ef,
  keep.firms = kl$keep.firms==T,
  cpc.sectors = cpc,
  keep.cpc = kl$keep.cpc==T,
  hs.codes = hs,
  keep.hs = kl$keep.hs==T,
  hit.brackets = hb,
  intervention.ids = int.id,
  keep.interventions = kl$keep.interventions==T,
  lag.adjustment=lag,
  trade.statistic=trade.statistic,
  trade.data=trade.data,
  intra.year.duration = kl$adjust.for.intra==T,
  rdata = FALSE,
  xlsx = TRUE,
  output.path = paste("17 Shiny/4 data kitchen/results/",Sys.Date()," - GTA data dish #",kl$ticket.number,".xlsx",sep="")
)

rm(c.period,gta.eval,a.flow,imps,exps,ij,ij.role,a.period,i.period,r.period,ift,i.types,mast,il,ef,cpc,hs,int.id,lag)
rm(kl, trade.coverage.estimates, bilateral.trade,parameter.choices)

if (error.message[1] == T) {
  error.message <- error.message
} else {
  error.message <- c(F,"")
}},

error = function(error.msg) {
  if(error.message[1]==T){
    error.message <<- c(T, stop.print)
  } else {
    error.message <<- c(T,error.msg$message)
  }
})
