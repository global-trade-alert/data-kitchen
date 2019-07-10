# # UNCOMMENT FOR TESTING
#
# library(gtalibrary)
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
# load("0 dev/data-kitchen-pb/log/kitchen log.Rdata")
# kl = kitchen.log[nrow(kitchen.log)-1,]

# PROCESSING

# path = "17 Shiny/4 data kitchen/"
path = "0 dev/data-kitchen-pb/"

library(xlsx)

tryCatch({
  
  if(as.character(kl$gta.evaluation)!=""){
    gta.eval=paste(unlist(strsplit(as.character(kl$gta.evaluation),",")))}else{gta.eval=NULL}
  
  if(as.character(kl$affected.flows)!=""){
    a.flow=paste(unlist(strsplit(as.character(kl$affected.flows),",")))}else{a.flow=NULL}
  
  if(as.character(kl$implementers)!=""){
    ij=paste(unlist(strsplit(as.character(kl$implementers),",")))
    ij <- ij[ij != ""]}else{ij=NULL}
  
  if(as.character(kl$affected)!=""){
    aff=paste(unlist(strsplit(as.character(kl$affected),",")))
    aff <- aff[aff != ""]}else{aff=NULL}
  
  if(as.character(kl$incl.affected.strictness)!=""){
    incl.aff.str=paste(unlist(strsplit(as.character(kl$incl.affected.strictness),",")))}else{incl.aff.str=NULL}
  
  if(as.character(kl$nr.affected)!=""){
    nr.aff=as.numeric(paste(unlist(strsplit(as.character(kl$nr.affected),","))))
    nr.aff <- nr.aff[is.na(nr.aff)==F]}else{nr.aff=c(0,999)}
  
  if(as.character(kl$nr.affected.incl)!=""){
    nr.aff.incl=paste(unlist(strsplit(as.character(kl$nr.affected.incl),",")))}else{nr.aff.incl=NULL}
  
  
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
  
  if(as.character(kl$in.force.today)!=""){
    ift=paste(unlist(strsplit(as.character(kl$in.force.today),",")))}else{ift=NULL}
  
  if(as.character(kl$intervention.types)!=""){
    i.types=paste(unlist(strsplit(as.character(kl$intervention.types),",")))
    i.types <- i.types[i.types != ""]
    nes.list <- c("Import-related non-tariff measure","Export-related non-tariff measure","Public procurement","State aid","FDI: Treatment and operations")
    if (any(i.types %in% nes.list)){
      for (i in 1:length(i.types)){
        if (i.types[i] %in% nes.list){i.types[i] <- paste0(i.types[i],", nes")}
        if (i.types[i] ==" nes"){i.types[i] <- 0}}
      i.types <- i.types[i.types!=0]
    }}else{i.types=NULL}
  
  if(as.character(kl$mast.chapters)!=""){
    mast=paste(unlist(strsplit(as.character(kl$mast.chapters),",")))
    mast <- mast[mast != ""]}else{mast=NULL}
  
  if(! grepl("any", as.character(kl$implementation.level))){
    il=paste(unlist(strsplit(as.character(kl$implementation.level),",")))}else{il=NULL}
  
  if(! grepl("any", as.character(kl$eligible.firms))){
    ef=paste(unlist(strsplit(as.character(kl$eligible.firms),",")))}else{ef=NULL}
  
  
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
  
  
  # CHECK IF "ALL" VALUE IS INCLUDED ANYWHERE
  # THESE WILL BE CALCULATED SEPARATELY IN A SECOND LOOP
  additional.all <- c()
  
  types = list("aff" = aff,
               "ij" = ij,
               "cpc" = cpc,
               "hs" = hs,
               "mast" = mast,
               "i.types" = i.types)
  
  for (i in 1:length(types)) {
    if(length(types[[i]]) == 1 & types[[i]] == "all") {
      eval(parse(text=paste0(names(types[i]),"= NULL")))  
    } else if ("all" %in% types[[i]]) {
      additional.all <- c(additional.all, names(types[i]))
      eval(parse(text=paste0(names(types[i]),"=",names(types[i]),"[",names(types[i]),"!= 'all']")))
    }  
  }
  
  # CONVERT HS AND CPC TO NUMERIC IF NECESSARY
  if (is.null(hs)==F){hs <- as.numeric(hs)}
  if (is.null(cpc)==F){cpc <- as.numeric(cpc)}
  
  ## running the function
  gta_data_slicer(
    data.path="data/master_plus.Rdata",
    gta.evaluation= gta.eval,
    affected.flows = a.flow,
    implementing.country = ij,
    keep.implementer = kl$keep.implementer==T,
    affected.country = aff,
    keep.affected = kl$keep.affected==T,
    incl.affected.strictness = incl.aff.str,
    keep.others = kl$keep.others==T,
    nr.affected = nr.aff, 
    nr.affected.incl = nr.aff.incl,
    announcement.period = a.period,
    implementation.period = i.period,
    keep.implementation.na = kl$keep.implementation.na == T,
    revocation.period = r.period,
    keep.revocation.na = kl$keep.revocation.na == T,
    submission.period = s.period,
    in.force.today = ift,
    intervention.types = i.types,
    keep.type = kl$keep.type==T,
    mast.chapters = mast,
    keep.mast = kl$keep.mast==T,
    implementation.level = il,
    keep.level = kl$keep.level==T,
    eligible.firms = ef,
    keep.firms = kl$keep.firms==T,
    cpc.sectors = cpc,
    keep.cpc = kl$keep.cpc==T,
    hs.codes = hs,
    keep.hs = kl$keep.hs==T,
    intervention.ids = int.id,
    keep.interventions = kl$keep.interventions==T,
    lag.adjustment=lag
  )
  
  master <- list(master.sliced)
  p.choices <- list(parameter.choice.slicer)
  
  # Only make interventions list from selected part, because including "All" values would make it too large
  if (kl$interventions.list==T) {
    
    interventions.list <- master.sliced
    interventions.list$url <- paste("https://www.globaltradealert.org/state-act/", interventions.list$state.act.id, sep="")
    interventions.list <- interventions.list[,c("intervention.id","url","implementing.jurisdiction","intervention.type","gta.evaluation","title")]
    names(interventions.list) <- c("ID","URL","Implementer","Instrument","Evaluation","Title")
    interventions.list <- unique(interventions.list)
    
    # eval(parse(text=paste0("xlsxList = list('",sheetnames.int[i],"' = interventions.list, '",sheetnames.int[i+2],"' = parameter.choice.slicer)")))
    # xlsxList <- list(paste0(sheetnames.int[i]) = interventions.list, paste0(sheetnames.int[i+2]) = parameter.choice.slicer)
    write.xlsx(x=interventions.list, file = paste(path,"results/Interventions list from ", Sys.Date(),".xlsx", sep=""), sheetName = "Interventions list", rowNames = F, append = F)
    write.xlsx(x=parameter.choice.slicer, file = paste(path,"results/Interventions list from ", Sys.Date(),".xlsx", sep=""), sheetName = "Parameter choices", append = T, row.names = F)
    rm(interventions.list)
  }
  
  
  # CALCULATE ALL VALUES IF NECESSARY
  if(length(additional.all)>0) {
    
    # MAKE SECOND KL ROW TO BE ABLE TO CHANGE VALUE IN IT WHERE NECESSARY
    kl2 <- as.data.frame(kl)
    
    types = list(c("aff", "group.affected","Affected jurisdictions"),
                 c("ij", "group.implementer","Implementing jurisdiction"),
                 c("cpc", "group.cpc","CPC Sectors"),
                 c("hs", "group.hs","HS Codes"),
                 c("mast", "group.mast","MAST Chapters"),
                 c("i.types", "group.types","Intervention types"))
    
    for (i in 1:length(types)) {
      if(types[[i]][1] %in% additional.all) {
        eval(parse(text=paste0(types[[i]][1],"= NULL")))
        eval(parse(text=paste0("kl2$",types[[i]][2],"= TRUE")))
      }
    }
    
    ## running the function
    gta_data_slicer(
      data.path="data/master_plus.Rdata",
      gta.evaluation= gta.eval,
      affected.flows = a.flow,
      implementing.country = ij,
      keep.implementer = kl$keep.implementer==T,
      affected.country = aff,
      keep.affected = kl$keep.affected==T,
      incl.affected.strictness = incl.aff.str,
      keep.others = kl$keep.others==T,
      nr.affected = nr.aff, 
      nr.affected.incl = nr.aff.incl,
      announcement.period = a.period,
      implementation.period = i.period,
      keep.implementation.na = kl$keep.implementation.na == T,
      revocation.period = r.period,
      keep.revocation.na = kl$keep.revocation.na == T,
      submission.period = s.period,
      in.force.today = ift,
      intervention.types = i.types,
      keep.type = kl$keep.type==T,
      mast.chapters = mast,
      keep.mast = kl$keep.mast==T,
      implementation.level = il,
      keep.level = kl$keep.level==T,
      eligible.firms = ef,
      keep.firms = kl$keep.firms==T,
      cpc.sectors = cpc,
      keep.cpc = kl$keep.cpc==T,
      hs.codes = hs,
      keep.hs = kl$keep.hs==T,
      intervention.ids = int.id,
      keep.interventions = kl$keep.interventions==T,
      lag.adjustment=lag
    )
    
    master <- list(master, master.sliced)
    p.choices <- list(p.choices, parameter.choice.slicer)
    kitchen.rows <- list(kl, kl2)
  }
  
  
  if (error.message[1] == T) {
    error.message <- error.message
  } else {
    
    error.message <<- c(F,"")
    
    rm(gta.eval,a.flow,ij,aff,a.period,i.period,r.period,s.period,ift,i.types,mast,il,ef,cpc,hs,int.id,lag, a.a.nr)
    
    
    # Helper vector to append values to excel file
    append.helper <- c(FALSE, TRUE)
    sheetnames.int <- c("Interventions List", "Interv. List (All)", "Parameter choices", "Param Ch. (All)")
    sheetnames.counts <- c("Count statistics", "Count statistics (All)", "Parameter choices", "Param Ch. (All)")
    
    # Processing of sliced master dataset
    
    for (i in 1:length(master)){
      
      
      kl <- kitchen.rows[[i]]
      master.sliced <- as.data.frame(master[[i]])
      parameter.choice.slicer <- as.data.frame(p.choices[[i]])
      
      if (kl$aggregate.y=="affected.product") {
        master.sliced <- cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=",")
      }
      
      if (kl$aggregate.y=="affected.sector" | kl$group.cpc == F) {
        master.sliced <- cSplit(master.sliced, which(colnames(master.sliced)=="affected.sector"), direction="long", sep=",")
      }
      
      if (grepl("gta.evaluation.harmful.liberalising", as.character(kl$aggregate.x))){
        master.sliced$gta.evaluation[master.sliced$gta.evaluation %in% c("Red","Amber")] = "Harmful"
        master.sliced$gta.evaluation[master.sliced$gta.evaluation =="Green"] = "Liberalising"
        names(master.sliced)[names(master.sliced) == 'gta.evaluation'] <- 'gta.evaluation.harmful.liberalising'
      }
      
      
      if(as.character(kl$aggregate.y)!=""){
        agg.y=paste(unlist(strsplit(as.character(kl$aggregate.y),",")))}else{agg.y=NULL}
      
      if(as.character(kl$aggregate.x)!=""){
        agg.x=paste(unlist(strsplit(as.character(kl$aggregate.x),",")))
      }else{
        stop("Please specify at least one X variable for aggregation.")
      }
      
      # CHECK IF OTHER PARAMETERS SHALL BE GROUPED OR NOT
      if(kl$group.implementer == F) {
        agg.x <- append(agg.x, "implementing.jurisdiction")
      }
      
      if(kl$group.affected == F) {
        agg.x <- append(agg.x, "affected.jurisdiction")
      }
      
      if(kl$group.mast == F) {
        agg.x <- append(agg.x, "mast.chapter")
      }
      
      if(kl$group.type == F) {
        agg.x <- append(agg.x, "intervention.type")
      }
      
      if(kl$group.cpc == F) {
        agg.x <- append(agg.x, "affected.sector")
      }
      
      if(as.character(kl$aggregate.style)!=""){
        agg.style=paste(unlist(strsplit(as.character(kl$aggregate.style),",")))}else{agg.style=NULL}
      
      
      agg.x <- unique(agg.x)
      eval(parse(text=paste0("agg <- aggregate(",agg.y," ~ ",paste(agg.x, collapse = "+"), ", master.sliced, function(x) ", agg.style, ")")))
      
      agg[is.na(agg)] <- 0
      
      # Backup "agg" for later use with plots
      if(i==1){agg.temp <- agg}
      
      if (kl$xlsx == T) {
        
        if( (length(agg.x)>1) & TRUE %in% grepl("year", agg.x) ){
          
          agg.xlsx=reshape(agg, idvar=agg.x[grepl("year", agg.x)==F], timevar =agg.x[grepl("year", agg.x)==T] , direction="wide")
          
          names(agg.xlsx)=gsub(paste(as.character(agg.y),".", sep=""),"", names(agg.xlsx))
          
          agg.xlsx[is.na(agg.xlsx)]=0
          # eval(parse(text=paste0("xlsxList = list('",sheetnames.counts[i],"' = agg.xlsx, '",sheetnames.counts[i+2],"' = parameter.choice.slicer)")))
          # xlsxList = list(paste0(sheetnames.counts[i]) = agg.xlsx, paste0(sheetnames.counts[i+2]) = parameter.choice.slicer)
          write.xlsx(x=agg.xlsx, file = paste(path,"results/Count statistics from ", Sys.Date(),".xlsx", sep=""), sheetName = sheetnames.counts[i], row.names = F, append = append.helper[i])
          write.xlsx(x=parameter.choice.slicer, file = paste(path,"results/Count statistics from ", Sys.Date(),".xlsx", sep=""), sheetName = sheetnames.counts[i+2], append=T, row.names = F)
          
          
        } else{
          # eval(parse(text=paste0("xlsxList = list('",sheetnames.counts[i],"' = agg, '",sheetnames.counts[i+2],"' = parameter.choice.slicer)")))
          # xlsxList = list(paste0(sheetnames.counts[i]) = agg, paste0(sheetnames.counts[i+2]) = parameter.choice.slicer)
          write.xlsx(x=agg, file = paste(path,"results/Count statistics from ", Sys.Date(),".xlsx", sep=""), sheetName = sheetnames.counts[i],row.names = F, append = append.helper[i])
          write.xlsx(x=parameter.choice.slicer, file = paste(path,"results/Count statistics from ", Sys.Date(),".xlsx", sep=""), sheetName = sheetnames.counts[i+2], append=T, row.names = F)
          
          
        }
        
      }
      
    }
    
    gta_colour_palette()
    
    # Retrieve agg from backup
    agg <- agg.temp
    
    # MAP OUTPUT
    if (kl$map == T) {
      
      if(as.character(kl$map.title)!=""){
        title=paste(unlist(strsplit(as.character(kl$map.title),",")))}else{title=NULL}
      
      if(as.character(kl$map.legend.title)!=""){
        legend.title=paste(unlist(strsplit(as.character(kl$map.legend.title),",")))}else{legend.title=NULL}
      
      if(as.character(kl$map.colour.low)!=""){
        eval(parse(text=paste0("colour.low <- ",kl$map.colour.low)))}else{colour.low=NULL}
      
      if(as.character(kl$map.colour.high)!=""){
        eval(parse(text=paste0("colour.high <- ",kl$map.colour.high)))}else{colour.high=NULL}
      
      if(as.character(kl$map.countries)!=""){
        countries=paste(unlist(strsplit(as.character(kl$map.countries),",")))}else{countries=NULL}
      
      if(as.character(kl$map.value)!=""){
        value=paste(unlist(strsplit(as.character(kl$map.value),",")))}else{value=NULL}
      
      if(as.character(kl$map.splits)!=""){
        range.split=as.numeric(paste(unlist(strsplit(as.character(kl$map.splits),","))))}else{value=waiver()}
      
      if(as.character(kl$map.brackets)!=""){
        range.split=as.numeric(paste(unlist(strsplit(as.character(kl$map.brackets),","))))}
      
      
      
      eval(parse(text=paste0("data <- aggregate(`",value,"` ~ `",countries,"`, agg, function(x) sum(x))")))
      
      map <- gta_plot_map(data = data, countries = countries, value = value, title = title, legend.title = legend.title, colour.high = colour.high, colour.low = colour.low, range.split = range.split)
      
      gta_plot_saver(plot = map, path = paste0(path,"results"), name = paste("Map chart from ", Sys.Date(), sep=""), eps=F)
      
      rm(map, colour.low, colour.high, value, countries, legend.title)
      
    }
    
    # TILE CHART OUTPUT
    if (kl$tile == T) {
      
      if(as.character(kl$tile.title)!=""){
        title=paste(unlist(strsplit(as.character(kl$tile.title),",")))}else{title=NULL}
      
      if(as.character(kl$tile.legend.title)!=""){
        legend.title=paste(unlist(strsplit(as.character(kl$tile.legend.title),",")))}else{legend.title=NULL}
      
      if(as.character(kl$tile.x.axis.title)!=""){
        x.axis=paste(unlist(strsplit(as.character(kl$tile.x.axis.title),",")))}else{x.axis=NULL}
      
      if(as.character(kl$tile.y.axis.title)!=""){
        y.axis=paste(unlist(strsplit(as.character(kl$tile.y.axis.title),",")))}else{y.axis=NULL}
      
      if(as.character(kl$tile.colour.low)!=""){
        eval(parse(text=paste0("colour.low <- ",kl$tile.colour.low)))}else{colour.low=NULL}
      
      if(as.character(kl$tile.colour.high)!=""){
        eval(parse(text=paste0("colour.high <- ",kl$tile.colour.high)))}else{colour.high=NULL}
      
      if(as.character(kl$tile.x.var)!=""){
        x.var=paste(unlist(strsplit(as.character(kl$tile.x.var),",")))}else{x.var=NULL}
      
      if(as.character(kl$tile.y.var)!=""){
        y.var=paste(unlist(strsplit(as.character(kl$tile.y.var),",")))}else{y.var=NULL}
      
      if(as.character(kl$tile.value.var)!=""){
        value=paste(unlist(strsplit(as.character(kl$tile.value.var),",")))}else{value=NULL}
      
      eval(parse(text=paste0("data <- aggregate(`",value,"` ~ `",x.var,"`+`",y.var,"`, agg, function(x) sum(x))")))
      
      tile <- gta_plot_tile(data = data, value.x = x.var, value.y = y.var, values = value, title = title, legend.title = legend.title, colour.low = colour.low, colour.high = colour.high, y.axis.name = y.axis, x.axis.name = x.axis)
      
      gta_plot_saver(plot = tile, path = paste0(path,"results"), name = paste("Tile chart from ", Sys.Date(), sep=""), eps=F)
      
      rm(tile, colour.low, colour.high, value, title, legend.title, x.var, y.var, x.axis, y.axis)
      
    }
    
    
    # LINE CHART OUTPUT
    if (kl$line == T) {
      
      if(as.character(kl$line.title)!=""){
        title=paste(unlist(strsplit(as.character(kl$line.title),",")))}else{title=NULL}
      
      if(as.character(kl$line.legend.title)!=""){
        legend.title=paste(unlist(strsplit(as.character(kl$line.legend.title),",")))}else{legend.title=NULL}
      
      if(as.character(kl$line.x.axis.title)!=""){
        x.axis=paste(unlist(strsplit(as.character(kl$line.x.axis.title),",")))}else{x.axis=NULL}
      
      if(as.character(kl$line.y.axis.title)!=""){
        y.axis=paste(unlist(strsplit(as.character(kl$line.y.axis.title),",")))}else{y.axis=NULL}
      
      if(as.character(kl$line.x.var)!=""){
        x.var=paste(unlist(strsplit(as.character(kl$line.x.var),",")))}else{x.var=NULL}
      
      if(as.character(kl$line.y.var)!=""){
        y.var=paste(unlist(strsplit(as.character(kl$line.y.var),",")))}else{y.var=NULL}
      
      if(as.character(kl$line.group.var)!=""){
        group=paste(unlist(strsplit(as.character(kl$line.group.var),",")))}else{group=NULL}
      
      if(as.character(kl$line.colour)=="gta_colour$qualitative"){
        eval(parse(text=paste0("colour <- ",kl$line.colour)))
        
      } else if(as.character(kl$line.colour)!="gta_colour$qualitative"){
        eval(parse(text=paste0("colour <- ",kl$line.colour,"(length(unique(agg$'",group,"')))")))
        
      }else{colour=NULL}
      
      if (is.null(group)==F) {
        eval(parse(text=paste0("data <- aggregate(`",y.var,"` ~ `",x.var,"`+`",group,"`, agg, function(x) sum(x))"))) } else {
          eval(parse(text=paste0("data <- aggregate(`",y.var,"` ~ `",x.var,"`, agg, function(x) sum(x))"))) }
      
      data[,c("value.x","value.y")] <- data[,c(x.var, y.var)]
      
      data$value.x.labels = data$value.x
      data$value.y.labels = data$value.y
      data$value.x.breaks = data$value.x
      data$value.y.breaks = data$value.y
      
      if (is.numeric(data$value.x)==F) {
        data$value.x.breaks = as.numeric(data$value.x.breaks)
        data$value.x.breaks.temp <- data$value.x.breaks
        i = 1
        for (h in unique(as.numeric(data$value.x))) {
          data$value.x.breaks[data$value.x.breaks.temp==h] <-  i
          i=i+1
        }
        data$value.x.breaks.temp <- NULL
      }
      
      if (is.numeric(data$value.y)==F) {
        data$value.y.breaks = as.numeric(data$value.y.breaks)
        data$value.y.breaks.temp <- data$value.y.breaks
        i = 1
        for (h in unique(as.numeric(data$value.y))) {
          data$value.y.breaks[data$value.y.breaks.temp==h] <-  i
          i=i+1
        }
        data$value.y.breaks.temp <- NULL
      }
      
      if (is.null(group)==T) {
        
        line <- ggplot() +
          geom_line(data=data, aes(x=value.x.breaks, y=value.y.breaks))+
          scale_y_continuous(breaks = waiver(), labels = waiver(), sec.axis = sec_axis(~.,breaks = waiver(), name = y.axis, labels = waiver()))+
          scale_x_continuous(breaks = unique(data$value.x.breaks), labels = unique(data$value.x.labels))+
          ggtitle(title)+
          labs(x=x.axis, y=y.axis)+
          gta_theme(x.bottom.angle = 45,
                    x.bottom.align = 1)
      }
      
      
      if (is.null(group)==F) {
        
        data[,c("group")] <- data[,c(group)]
        
        data$group <- factor(data$group)
        eval(parse(text=paste0("col <- round(sqrt((length(unique(agg$'",group,"')))),digits=0)")))
        
        line <- ggplot() +
          geom_line(data=data, aes(x=value.x.breaks, y=value.y.breaks, colour=group))+
          scale_y_continuous(breaks = waiver(), labels = waiver(), sec.axis = sec_axis(~.,breaks = waiver(), name = y.axis, labels = waiver()))+
          scale_x_continuous(breaks = unique(data$value.x.breaks), labels = unique(data$value.x.labels))+
          scale_colour_manual(values=colour, labels=unique(data$group))+
          ggtitle(title)+
          labs(x=x.axis, y=y.axis)+
          guides(colour = guide_legend(title=legend.title, ncol = col))+
          gta_theme(x.bottom.angle = 45,
                    x.bottom.align = 1)
      }
      
      gta_plot_saver(plot = line, path = paste0(path,"results"), name = paste("Line chart from ", Sys.Date(), sep=""), eps=F)
      
      rm(agg.line, line, colour, group, title, legend.title, x.var, y.var, x.axis, y.axis, col)
      
    }
    
    # BAR CHART OUTPUT
    if (kl$bar == T) {
      
      if(as.character(kl$bar.title)!=""){
        title=paste(unlist(strsplit(as.character(kl$bar.title),",")))}else{title=NULL}
      
      if(as.character(kl$bar.legend.title)!=""){
        legend.title=paste(unlist(strsplit(as.character(kl$bar.legend.title),",")))}else{legend.title=NULL}
      
      if(as.character(kl$bar.x.axis.title)!=""){
        x.axis=paste(unlist(strsplit(as.character(kl$bar.x.axis.title),",")))}else{x.axis=NULL}
      
      if(as.character(kl$bar.y.axis.title)!=""){
        y.axis=paste(unlist(strsplit(as.character(kl$bar.y.axis.title),",")))}else{y.axis=NULL}
      
      if(as.character(kl$bar.colour)!=""){
        eval(parse(text=paste0("colour <- ",kl$bar.colour)))}else{colour=NULL}
      
      if(as.character(kl$bar.x.var)!=""){
        x.var=paste(unlist(strsplit(as.character(kl$bar.x.var),",")))}else{x.var=NULL}
      
      if(as.character(kl$bar.y.var)!=""){
        y.var=paste(unlist(strsplit(as.character(kl$bar.y.var),",")))}else{y.var=NULL}
      
      if(as.character(kl$bar.group.var)!=""){
        group=paste(unlist(strsplit(as.character(kl$bar.group.var),",")))}else{group=NULL}
      
      if(as.character(kl$bar.colour)=="gta_colour$qualitative"){
        eval(parse(text=paste0("colour <- ",kl$bar.colour)))
        
      } else if(as.character(kl$bar.colour)!="gta_colour$qualitative"){
        eval(parse(text=paste0("colour <- ",kl$bar.colour,"(length(unique(agg$'",group,"')))")))
        
      }else{colour=NULL}
      
      if (is.null(group)==F) {
        eval(parse(text=paste0("data <- aggregate(`",y.var,"` ~ `",x.var,"`+`",group,"`, agg, function(x) sum(x))"))) } else {
          eval(parse(text=paste0("data <- aggregate(`",y.var,"` ~ `",x.var,"`, agg, function(x) sum(x))"))) }
      
      data[,c("value.x","value.y")] <- data[,c(x.var, y.var)]
      
      data$value.x.labels = data$value.x
      data$value.y.labels = data$value.y
      data$value.x.breaks = data$value.x
      data$value.y.breaks = data$value.y
      
      if (is.numeric(data$value.x)==F) {
        data$value.x.breaks = as.numeric(data$value.x.breaks)
        data$value.x.breaks.temp <- data$value.x.breaks
        i = 1
        for (h in unique(as.numeric(data$value.x))) {
          data$value.x.breaks[data$value.x.breaks.temp==h] <-  i
          i=i+1
        }
        data$value.x.breaks.temp <- NULL
      }
      
      if (is.numeric(data$value.y)==F) {
        data$value.y.breaks = as.numeric(data$value.y.breaks)
        data$value.y.breaks.temp <- data$value.y.breaks
        i = 1
        for (h in unique(as.numeric(data$value.y))) {
          data$value.y.breaks[data$value.y.breaks.temp==h] <-  i
          i=i+1
        }
        data$value.y.breaks.temp <- NULL
      }
      
      if (is.null(group)==T) {
        
        bar <- ggplot() +
          geom_bar(data=data, aes(x=value.x.breaks, y=value.y.breaks), stat = "identity", fill=gta_colour$blue[1])+
          scale_y_continuous(breaks = waiver(), labels = waiver(), sec.axis = sec_axis(~.,breaks = waiver(), name = y.axis, labels = waiver()))+
          scale_x_continuous(breaks = unique(data$value.x.breaks), labels = unique(data$value.x.labels))+
          ggtitle(title)+
          labs(x=x.axis, y=y.axis)+
          gta_theme(x.bottom.angle = 45,
                    x.bottom.align = 1)
      }
      
      if (is.null(group)==F) {
        
        data[,c("group")] <- data[,c(group)]
        data$group <- factor(data$group)
        
        eval(parse(text=paste0("col <- round(sqrt((length(unique(agg$'",group,"')))),digits=0)")))
        
        bar <- ggplot() +
          geom_bar(data=data, aes(x=value.x.breaks, y=value.y.breaks, fill = group), stat = "identity")+
          scale_y_continuous(breaks = waiver(), labels = waiver(), sec.axis = sec_axis(~.,breaks = waiver(), name = y.axis, labels = waiver()))+
          scale_x_continuous(breaks = unique(data$value.x.breaks), labels = unique(data$value.x.labels))+
          scale_fill_manual(values=colour, labels=unique(data$group))+
          ggtitle(title)+
          labs(x=x.axis, y=y.axis)+
          guides(fill = guide_legend(title=legend.title, ncol = col))+
          gta_theme(x.bottom.angle = 45,
                    x.bottom.align = 1)
      }
      
      gta_plot_saver(plot = bar, path = paste0(path,"results"), name = paste("Bar chart from ", Sys.Date(), sep=""), eps=F)
      
      rm(agg.bar, bar, colour, group, title, legend.title, x.var, y.var, x.axis, y.axis, col)
    }
    
  }},
  error = function(error.msg) {
    if(error.message[1]==T){
      error.message <<- c(T, stop.print)
    } else {
      error.message <<- c(T,error.msg$message)
    }
  })

# rm(master.sliced, parameter.choice.slicer, gta_colour)
