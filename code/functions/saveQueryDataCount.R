saveQueryDataCount <- function(session = session, path = NULL, title = NULL, description = NULL, input = NULL) {

    # GET VALUE FROM FIELDS
    x <- input
    test <- data.frame(
      values = lapply(x, function(y) paste(unlist(y), collapse = ","))
    )
    
    test2 <<- test
    test <- test2
    test <- test %>% select(ends_with(".data.count"))
    test <- test %>% select(-ends_with(".selectized"))
    names(test)=gsub("values.", "", names(test))
    test <- gather(test, type, value, 1:ncol(test))
    # test <- subset(test, value != "")
    
    #REMOVE UNNECESSARY FIELDS AS THESE UPSET THE UPDATE FUNCTION
    test <- subset(test,  type %in% c("gta.evaluation.data.count",
                                      "affected.flows.data.count",
                                      "implementers.data.count",
                                      "keep.implementer.data.count",
                                      "group.implementer.data.count",
                                      "affected.data.count",
                                      "keep.affected.data.count",
                                      "group.affected.data.count",
                                      "keep.others.data.count",
                                      "incl.affected.strictness.data.count",
                                      "nr.affected.data.count",
                                      "nr.affected.incl.data.count",
                                      "in.force.today.data.count",
                                      "announcement.period.data.count",
                                      "implementation.period.data.count",
                                      "keep.implementation.na.data.count",
                                      "revocation.period.data.count",
                                      "keep.revocation.na.data.count",
                                      "submission.period.data.count",
                                      "intervention.types.data.count",
                                      "keep.type.data.count",
                                      "group.type.data.count",
                                      "mast.chapters.data.count",
                                      "keep.mast.data.count",
                                      "group.mast.data.count",
                                      "implementation.level.data.count",
                                      "keep.level.data.count",
                                      "eligible.firms.data.count",
                                      "keep.firms.data.count",
                                      "cpc.sectors.data.count",
                                      "keep.cpc.data.count",
                                      "group.cpc.data.count",
                                      "all.service.cpc.data.count",
                                      "hs.codes.data.count",
                                      "keep.hs.data.count",
                                      "intervention.ids.data.count",
                                      "keep.interventions.data.count",
                                      "lag.adjustment.data.count",
                                      "aggregate.y.data.count",
                                      "aggregate.x.data.count",
                                      "xlsx.data.count",
                                      "interventions.list.data.count",
                                      "line.data.count",
                                      "bar.data.count",
                                      "map.data.count",
                                      "tile.data.count",
                                      "line.title.data.count",
                                      "line.legend.title.data.count",
                                      "line.x.axis.title.data.count",
                                      "line.y.axis.title.data.count",
                                      "line.colour.data.count",
                                      "line.y.var.data.count",
                                      "line.group.var.data.count",
                                      "bar.title.data.count",
                                      "bar.legend.title.data.count",
                                      "bar.x.axis.title.data.count",
                                      "bar.y.axis.title.data.count",
                                      "bar.colour.data.count",
                                      "bar.y.var.data.count",
                                      "bar.group.var.data.count",
                                      "map.title.data.count",
                                      "map.legend.title.data.count",
                                      "map.colour.low.data.count",
                                      "map.colour.high.data.count",
                                      "map.countries.data.count",
                                      "map.value.data.count",
                                      "map.splits.data.count",
                                      "map.brackets.data.count",
                                      "tile.title.data.count",
                                      "tile.legend.title.data.count",
                                      "tile.x.axis.title.data.count",
                                      "tile.y.axis.title.data.count",
                                      "tile.colour.low.data.count",
                                      "tile.colour.high.data.count",
                                      "tile.x.var.data.count",
                                      "tile.y.var.data.count",
                                      "tile.value.var.data.count")
                   )
    
    row.names(test) <- NULL
    
    # GET NUMBER OF EXISTING QUERIES
    queries <- c()
    for(fct in list.files(paste0(path,"/code/modules/queries/datacounts/"), pattern = ".R", full.names=T)){
      queries <- c(queries, as.numeric(gsub(".R|q_dc","",basename(fct))))
    }
    
    # CREATE LINES WHICH WILL BE WRITTEN INTO THE QUERY FILE
    start <- paste0("q_dc",max(queries)+1," <- function(session=session, type=NULL) {\n if(is.null(type)==F){")
    title <- paste0("title <- '", title,"'\n","if(type=='title'){ return(title)}")
    description <- paste0("description <- '", description,"'\n","if(type=='description'){ return(description)}")
    updateCoverage <- "} else { \n\n updateDataCounts(session = session,"
    end <- ")}}"
    
    lines <- c()
    
    # THESE PARAMETERS MUST BE WRAPPEN IN A C() VECTOR
    listParameters <- c("implementers.data.count",
                        "affected.data.count",
                        "mast.chapters.data.count",
                        "cpc.sectors.data.count",
                        "implementation.level.data.count",
                        "gta.evaluation.data.count",
                        "implementer.role.data.count",
                        "intervention.types.data.count",
                        "revocation.period.data.count",
                        "hs.codes.data.count",
                        "announcement.period.data.count",
                        "implementation.period.data.count",
                        "submission.period.data.count",
                        "eligible.firms.data.count",
                        "aggregate.x.data.count")
    
    # EXTRACT PARAMETERS AND VALUES AND MAKE THEM WRITTABLE TO QUERY FILE
    for(i in 1:nrow(test)) {
      if (test[i,2] %in% c("TRUE","FALSE","NA")){ lines <- c(lines, paste0("    ",test[i,1], " = ",test[i,2],",")) 
      } else if (test[i,2] %in% c("NA,NA")) {lines <- c(lines, paste0("    ",test[i,1], " = c(",test[i,2],"),"))
      } else if (test[i,1] %in% listParameters) {lines <- c(lines, paste0("    ",test[i,1], " = c(",sprintf("'%s'", paste(unlist(paste0(unlist(strsplit(as.character(test[i,2]),",")))), collapse = "','")) ,"),"))
      } else if (test[i,1] %in% c("lag.adjustment.data.count") & test[i,2] == "") {lines <- c(lines, paste0("    ",test[i,1], " = NA,"))
      } else { lines <- c(lines, paste0("    ",test[i,1], " = '",test[i,2],"',")) }
    }
    
    lines[length(lines)] <- substr(lines[length(lines)],1,nchar(lines[length(lines)])-1)
    
    # WRITE QUERY FILE
    fileConn<-file(paste0(path,"code/modules/queries/datacounts/q_dc",max(queries)+1,".R"))
    writeLines(c(start,title,description,updateCoverage,lines,end), fileConn)
    close(fileConn)
    
    
    shinyjs::removeClass(selector = ".savequery-datacount", class = "active")

}