# UNCOMMENT FOR TESTING

# rm(list=ls())
# library(openxlsx)
# library(gtalibrary)
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
# load("17 Shiny/4 data kitchen/log/kitchen log.Rdata")
# kl = kitchen.log[1137,]

library(openxlsx)

# PROCESSING

path = "17 Shiny/4 data kitchen/"
# path = "0 dev/data-kitchen-pb/"

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
  
  if(as.character(kl$incl.importers.strictness)!=""){
    incl.imp.str=paste(unlist(strsplit(as.character(kl$incl.importers.strictness),",")))}else{incl.imp.str=NULL}
  
  if(as.character(kl$nr.importers)!=""){
    nr.imp=as.numeric(paste(unlist(strsplit(as.character(kl$nr.importers),","))))
    nr.imp <- nr.imp[is.na(nr.imp)==F]}else{nr.imp=c(0,999)}
  
  if(as.character(kl$nr.importers.incl)!=""){
    nr.imp.incl=paste(unlist(strsplit(as.character(kl$nr.importers.incl),",")))}else{nr.imp.incl=NULL}
  
  if(as.character(kl$exporters)!=""){
    exps=paste(unlist(strsplit(as.character(kl$exporters),",")))
    if("" %in% exps){exps = exps[!exps==""]}}else{exps=NULL}
  
  if(as.character(kl$incl.exporters.strictness)!=""){
    incl.exp.str=paste(unlist(strsplit(as.character(kl$incl.exporters.strictness),",")))}else{incl.exp.str=NULL}
  
  if(as.character(kl$nr.exporters)!=""){
    nr.exp=as.numeric(paste(unlist(strsplit(as.character(kl$nr.exporters),","))))
    nr.exp <- nr.exp[is.na(nr.exp)==F]}else{nr.exp=c(0,999)}
  
  if(as.character(kl$nr.exporters.incl)!=""){
    nr.exp.incl=paste(unlist(strsplit(as.character(kl$nr.exporters.incl),",")))}else{nr.exp.incl=NULL}
  
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
  
  if(as.character(kl$submission.period)!="NA,NA"){
    s.period=paste(unlist(strsplit(as.character(kl$submission.period),",")))
    s.period[s.period=="NA"] <- NA
  }else{s.period=NULL}
  
  if(as.character(kl$keep.in.force.on.date)!=""){
    ift=paste(unlist(strsplit(as.character(kl$keep.in.force.on.date),",")))}else{ift=NULL}
  
  if(as.character(kl$in.force.on.date)!=""){
    ifod=paste(as.character(kl$in.force.on.date),",")}else{ifod=NULL}
  
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
  
  # CHECK WHETHER ALL SERVICE OR ALL GOODS ARE CHOSEN
  if (any(c("all_service","all_goods") %in% unlist(strsplit(as.character(kl$cpc.sectors),",")))) {
    if ("all_service" %in% unlist(strsplit(as.character(kl$cpc.sectors),","))) {
      cpc_new <- subset(gtalibrary::cpc.names, cpc > 499 & cpc.digit.level == 3)$cpc
      temp <- unlist(strsplit(as.character(kl$cpc.sectors),","))
      temp <- temp[temp != "all_service"]
      temp <- append(temp, cpc_new)
      kl$cpc.sectors <- paste(temp, collapse = ",")
      rm(temp)
    }
    if ("all_goods" %in% unlist(strsplit(as.character(kl$cpc.sectors),","))) {
      cpc_new <- subset(gtalibrary::cpc.names, cpc < 500 & cpc.digit.level == 3)$cpc
      temp <- unlist(strsplit(as.character(kl$cpc.sectors),","))
      temp <- temp[temp != "all_goods"]
      temp <- append(temp, cpc_new)
      kl$cpc.sectors <- paste(temp, collapse = ",")
      rm(temp)
    }
  }
  
  # CHECK IF PRODUCT GROUPS ARE CHOSEN FOR CPC
  product.groups <- gtalibrary::hs.codes
  product.groups <- names(product.groups)
  product.groups <- product.groups[grepl("is.", product.groups)]
  product.groups <- gsub("is.","",product.groups)
  product.groups <- gsub("\\."," ",product.groups)
  
  if (any(tolower(c(product.groups)) %in% tolower(unlist(strsplit(as.character(kl$cpc.sectors),","))))) {
    groups <- product.groups[tolower(product.groups) %in% tolower(unlist(strsplit(as.character(kl$cpc.sectors),",")))]
    groups <- paste0("is.", tolower(gsub(" ","\\.",groups)))
    cpc_new <- c()
    for (i in groups) {
      eval(parse(text = paste0("temp <- subset(gtalibrary::hs.codes, ",i,"==T)$hs.code")))
      temp <- gta_hs_to_cpc(temp)
      temp <- unique(as.numeric(substr(temp, 1,3)))
      cpc_new <- c(cpc_new, temp)
    }
    temp <- unique(unlist(strsplit(as.character(kl$cpc.sectors),",")))
    temp <- unique(c(temp, cpc_new))
    kl$cpc.sectors <- paste(temp[! tolower(temp) %in% tolower(product.groups)], collapse=",")
  }
  
  if(as.character(kl$cpc.sectors)!=""){
    cpc=paste(unlist(strsplit(as.character(kl$cpc.sectors),",")))
    cpc <- cpc[is.na(cpc)==F]}else{cpc=NULL}
  
  
  # CHECK WHETHER PRODUCT GROUPS ARE IN HS CODES
  if (any(tolower(c(product.groups)) %in% tolower(unlist(strsplit(as.character(kl$hs.codes),","))))) {
    groups <- product.groups[tolower(product.groups) %in% tolower(unlist(strsplit(as.character(kl$hs.codes),",")))]
    groups <- paste0("is.", tolower(gsub(" ","\\.",groups)))
    hs_new <- c()
    for (i in groups) {
      eval(parse(text = paste0("temp <- subset(gtalibrary::hs.codes, ",i,"==T)$hs.code")))
      temp <- gta_hs_code_check(temp)
      hs_new <- c(hs_new, temp)
    }
    temp <- unique(unlist(strsplit(as.character(kl$hs.codes),",")))
    temp <- unique(c(temp, hs_new))
    kl$hs.codes <- paste(temp[! tolower(temp) %in% tolower(product.groups)], collapse=",")
  }
  
  if(as.character(kl$hs.codes)!=""){
    hs=paste(unlist(strsplit(as.character(kl$hs.codes),",")))
    hs <- hs[is.na(hs)==F]}else{hs=NULL}
  
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
  
  
  
  # CHECK IF "ALL" VALUE IS INCLUDED ANYWHERE
  # THESE WILL BE CALCULATED SEPARATELY IN A SECOND LOOP
  additional.all <- c()
  
  types = list("imps" = imps,
               "exps" = exps,
               "ij" = ij,
               "cpc" = cpc,
               "hs" = hs,
               "mast" = mast,
               "i.types" = i.types)
  
  for (i in 1:length(types)) {
    if (is.null(types[[i]])==F) {
    if(length(types[[i]]) == 1 & types[[i]] == "all") {
      eval(parse(text=paste0(names(types[i]),"= NULL")))  
    } else if ("all" %in% types[[i]]) {
      additional.all <- c(additional.all, names(types[i]))
      eval(parse(text=paste0(names(types[i]),"=",names(types[i]),"[",names(types[i]),"!= 'all']")))
    }  
    }
  }
  
  # CONVERT HS AND CPC TO NUMERIC IF NECESSARY
  if (is.null(hs)==F){hs <- as.numeric(hs)}
  if (is.null(cpc)==F){cpc <- as.numeric(cpc)}
  
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
    incl.importers.strictness = incl.imp.str,
    separate.importer.groups = kl$separate.importer.groups==T,
    nr.importers = nr.imp, 
    nr.importers.incl = nr.imp.incl,
    exporters = exps,
    group.exporters = kl$group.exporters==T,
    keep.exporters = kl$keep.exporters==T,
    incl.exporters.strictness = incl.exp.str,
    separate.exporter.groups = kl$separate.exporter.groups==T,
    nr.exporters = nr.exp, 
    nr.exporters.incl = nr.exp.incl,
    implementers = ij,
    implementer.role = ij.role,
    keep.implementer = kl$keep.implementer==T,
    announcement.period = a.period,
    implementation.period = i.period,
    revocation.period = r.period,
    keep.revocation.na = kl$keep.revocation.na==T,
    submission.period = s.period,
    keep.in.force.on.date = ift,
    in.force.on.date = ifod,
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
    xlsx = FALSE,
    output.path = paste(path,"results/",Sys.Date()," - GTA data dish #",kl$ticket.number,".xlsx",sep=""),
    xlsx.interventions = kl$xlsx.interventions==T,
    output.path.interventions = paste(path,"results/",Sys.Date()," - GTA data dish #",kl$ticket.number," - interventions.xlsx",sep="")
  )
  
  results <- trade.coverage.estimates
  pm.choices <- parameter.choices
  
  # CALCULATE ALL VALUES IF NECESSARY
  if(length(additional.all)>0) {
    
    types = list(c("imps", "group.importers","Importers","Importing country"),
                 c("exps", "group.exporters","Exporters","Exporting country"),
                 c("ij", "group.implementers","Implementers","Implementers"),
                 c("cpc", "group.cpc","CPC Sectors","CPC Sectors"),
                 c("hs", "group.hs","HS Codes","HS Codes"),
                 c("mast", "group.mast","MAST chapters","MAST chapter name"),
                 c("i.types", "group.type","Intervention types","Intervention type"))
    
    for (i in 1:length(types)) {
      if(types[[i]][1] %in% additional.all) {
        eval(parse(text=paste0(types[[i]][1],"= NULL")))
        eval(parse(text=paste0("kl$",types[[i]][2],"= TRUE")))
        if(types[[i]][1]=="imps"){ kl$separate.importer.groups = F }
        if(types[[i]][1]=="exps"){ kl$separate.exporter.groups = F }
      }
    }
    
  
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
      incl.importers.strictness = incl.imp.str,
      separate.importer.groups = kl$separate.importer.groups==T,
      nr.importers = nr.imp, 
      nr.importers.incl = nr.imp.incl,
      exporters = exps,
      group.exporters = kl$group.exporters==T,
      keep.exporters = kl$keep.exporters==T,
      incl.exporters.strictness = incl.exp.str,
      separate.exporter.groups = kl$separate.exporter.groups==T,
      nr.exporters = nr.exp, 
      nr.exporters.incl = nr.exp.incl,
      implementers = ij,
      implementer.role = ij.role,
      keep.implementer = kl$keep.implementer==T,
      announcement.period = a.period,
      implementation.period = i.period,
      revocation.period = r.period,
      keep.revocation.na = kl$keep.revocation.na==T,
      submission.period = s.period,
      keep.in.force.on.date = ift,
      in.force.on.date = ifod,
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
      xlsx = FALSE,
      output.path = paste(path,"results/",Sys.Date()," - GTA data dish #",kl$ticket.number,".xlsx",sep="")
    )
    
    results.2 <- trade.coverage.estimates
    pm.choices.2 <- parameter.choices
    
    # for (i in 1:length(types)) {
    #   if(types[[i]][1] %in% additional.all) {
    #     eval(parse(text=paste0("results.2$`",types[[i]][4],"` = 'All ", types[[i]][3]," in the GTA'")))
    #     eval(parse(text=paste0("results$`",types[[i]][3],"` = 'Selected ", types[[i]][3],"'")))
    #   }
    # }

    # ADD INDICATOR COLUMNS FOR ALL/SELECTED
    for (i in 1:length(types)) {
      if(types[[i]][1] %in% additional.all) {
        if (types[[i]][4] %in% names(results)) {
          eval(parse(text=paste0("results.2$`",types[[i]][4],"` = 'All ", types[[i]][3]," in the GTA'")))
        } else {
          eval(parse(text=paste0("results.2$`",types[[i]][4],"` = 'All ", types[[i]][3]," in the GTA'")))
          eval(parse(text=paste0("results$`",types[[i]][4],"` = 'Selected ", types[[i]][3],"'")))
        }
      }
    }
    
    # CHECK IF NUMBER OF COLUMNS ARE DIFFERENT
    if (ncol(results) != ncol(results.2)) {
      names.diff <- (names(results)[! names(results) %in% names(results.2)])
     for (n in names.diff) {
       results.2[n] <- paste0("All ",n," in the GTA") 
     }
    }
    
    results$orderCol <- 1
    results.2$orderCol <- 2
    results <- rbind(results, results.2)
    rm(results.2)
  }
  
  # ADD "ALL" PARAMETER CHOICES TO A SINGLE PARAMTER CHOICES DF
  if (exists("pm.choices.2")) {
    for (r in 1:nrow(pm.choices)){
      second.value <- pm.choices.2$parameter[pm.choices.2$parameter == pm.choices[r,1]]
      if (pm.choices[r,2] != pm.choices.2$choice[pm.choices.2$parameter == second.value]) {
        pm.choices$choice[r] <- paste0(pm.choices$choice[r], " + All")
      }
    }
  }
  
  # ORDER COLUMNS AND ROWS
  results.names <- names(results)
  
  # order columns
  order.list <- c("orderCol",
                  "`Importing country`",
                  "`Exporting country`",
                  "`MAST chapter ID`",
                  "`MAST chapter name`",
                  "`CPC Sectors`",
                  "`HS Codes`",
                  "`Intervention type`",
                  "`Number of interventions affecting exported product`",
                  c(paste0("`Trade coverage estimate for ",seq(2000,2030,1),"`")))
  
  order.rows <- order.list[gsub("`","",order.list) %in% names(results)]
  eval(parse(text=paste0("results <- results[with(results, order(",paste0(order.rows, collapse = ", "),")),]")))
  
  # REMOVE ORDERCOL IF PRESENT
  if ("orderCol" %in% order.rows) {
    results$orderCol <- NULL
  }
  
  # ORDER COLUMNS
  order.list <- gsub("`","",order.list)
  order.list <- order.list[order.list %in% names(results)]
  results <- results[order.list]
  
  
  # SAVE EXCEL
  xlsxList <- list("Coverages"=results,"Parameter choices"=pm.choices)
  
  openxlsx::write.xlsx(xlsxList, file=paste(path,"results/",Sys.Date()," - GTA data dish #",kl$ticket.number,".xlsx",sep=""), rowNames = F)
  
  rm(c.period,gta.eval,a.flow,imps,nr.imp,nr.imp.incl,incl.imp.str,exps,nr.exp,nr.exp.incl,incl.exp.str,ij,ij.role,a.period,i.period,r.period,ift,i.types,mast,il,ef,cpc,hs,int.id,lag)
  rm(kl, trade.coverage.estimates, bilateral.trade,parameter.choices)
  
  
  if (error.message[1] == T) {
    error.message <- error.message
  } else {
    error.message <- c(F,"")
  }},
  
  error = function(error.msg) {
    if(error.message[1]==T){
      error.message <<- c(T, error.message[2])
    } else {
      error.message <<- c(T,error.msg$message)
    }
  })
