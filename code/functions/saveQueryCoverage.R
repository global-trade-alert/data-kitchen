saveQueryCoverage <- function(session = session, path = NULL, title = NULL, description = NULL, input = NULL) {

    # GET VALUE FROM FIELDS
    x <- input
    test <- data.frame(
      values = lapply(x, function(y) paste(unlist(y), collapse = ","))
    )
    
    test <- test %>% select(-ends_with(".data.count"))
    test <- test %>% select(-ends_with(".selectized"))
    names(test)=gsub("values.", "", names(test))
    test <- gather(test, type, value, 1:ncol(test))
    # test <- subset(test, value != "")
    
    #REMOVE UNNECESSARY FIELDS AS THESE UPSET THE UPDATE FUNCTION
    test <- subset(test,  type %in% c("gta.evaluation",
                                       "affected.flows",
                                       "implementers",
                                       "keep.implementer",
                                       "affected",
                                       "keep.affected",
                                       "keep.others",
                                       "in.force.today",
                                       "announcement.period",
                                       "implementation.period",
                                       "keep.implementation.na",
                                       "revocation.period",
                                       "keep.revocation.na",
                                       "submission.period",
                                       "intervention.types",
                                       "keep.type",
                                       "mast.chapters",
                                       "keep.mast",
                                       "implementation.level",
                                       "keep.level",
                                       "eligible.firms",
                                       "keep.firms",
                                       "cpc.sectors",
                                       "keep.cpc",
                                       "hs.codes",
                                       "keep.hs",
                                       "intervention.ids",
                                       "keep.interventions",
                                       "lag.adjustment",
                                       "coverage.period",
                                       "current.year.todate",
                                       "choose.tradebase",
                                       "choose.output",
                                       "adjust.for.intra",
                                       "exporters",
                                       "group.exporters",
                                       "keep.exporters",
                                       "incl.exporters.strictness",
                                       "nr.exporters",
                                       "nr.exporters.incl",
                                       "separate.exporter.groups",
                                       "importers",
                                       "group.importers",
                                       "keep.importers",
                                       "incl.importers.strictness",
                                       "nr.importers",
                                       "nr.importers.incl",
                                       "separate.importer.groups",
                                       "implementer.role",
                                       "nr.also.importers",
                                       "jointly.affected.importers",
                                       "nr.also.exporters",
                                       "jointly.affected.exporters",
                                       "hit.brackets",
                                       "group.type",
                                       "group.mast"))
    
    row.names(test) <- NULL
    
    # GET NUMBER OF EXISTING QUERIES
    queries <- c()
    for(fct in list.files(paste0(path,"/code/modules/queries/coverages"), pattern = ".R", full.names=T)){
      queries <- c(queries, as.numeric(gsub(".R|q","",basename(fct))))
    }
    
    # CREATE LINES WHICH WILL BE WRITTEN INTO THE QUERY FILE
    start <- paste0("q",max(queries)+1," <- function(session=session, type=NULL) {\n if(is.null(type)==F){")
    title <- paste0("title <- '", title,"'\n","if(type=='title'){ return(title)}")
    description <- paste0("description <- '", description,"'\n","if(type=='description'){ return(description)}")
    updateCoverage <- "} else { \n\n updateCoverages(session = session,"
    end <- ")}}"
    
    lines <- c()
    
    # THESE PARAMETERS MUST BE WRAPPEN IN A C() VECTOR
    listParameters <- c("implementers",
                        "importers",
                        "exporters",
                        "mast.chapters",
                        "cpc.sectors",
                        "implementation.level",
                        "gta.evaluation",
                        "implementer.role",
                        "intervention.types",
                        "revocation.period",
                        "hs.codes",
                        "announcement.period",
                        "implementation.period",
                        "submission.period",
                        "eligible.firms")
    
    # EXTRACT PARAMETERS AND VALUES AND MAKE THEM WRITTABLE TO QUERY FILE
    for(i in 1:nrow(test)) {
      if (test[i,2] %in% c("TRUE","FALSE","NA")){ lines <- c(lines, paste0("    ",test[i,1], " = ",test[i,2],",")) 
      } else if (test[i,2] %in% c("NA,NA")) {lines <- c(lines, paste0("    ",test[i,1], " = c(",test[i,2],"),"))
      } else if (test[i,1] %in% c("coverage.period")) {lines <- c(lines, paste0("    ",test[i,1], " = c(",test[i,2],"),"))
      } else if (test[i,1] %in% listParameters) {lines <- c(lines, paste0("    ",test[i,1], " = c(",sprintf("'%s'", paste(unlist(paste0(unlist(strsplit(as.character(test[i,2]),",")))), collapse = "','")) ,"),"))
      } else if (test[i,1] %in% c("lag.adjustment") & test[i,2] == "") {lines <- c(lines, paste0("    ",test[i,1], " = NA,"))
      } else { lines <- c(lines, paste0("    ",test[i,1], " = '",test[i,2],"',")) }
    }
    
    lines[length(lines)] <- substr(lines[length(lines)],1,nchar(lines[length(lines)])-1)
    
    # WRITE QUERY FILE
    fileConn<-file(paste0(path,"code/modules/queries/coverages/q",max(queries)+1,".R"))
    writeLines(c(start,title,description,updateCoverage,lines,end), fileConn)
    close(fileConn)
    
    
    shinyjs::removeClass(selector = ".savequery", class = "active")

}