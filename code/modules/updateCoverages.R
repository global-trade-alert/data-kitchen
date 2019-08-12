callUpdateCoverages <- function(session,
                                query = NULL,
                                path = NULL) {
  
  if(is.null(path)) {
    stop("Please specify path")
  }
  
  for(fct in list.files(paste0(path,"/code/modules/queries/coverages"), pattern = ".R", full.names=T)){
    source(fct)
  }
  
  if (is.null(query)==F) {
    
    if (query == "Unset") {
      
      updateCoverages(session = session)
      
    } else {
    
    updateCoverages(session = session)
    # print(paste0("q",query,"(session = session)"))
    eval(parse(text=paste0(query,"(session = session)")))
    # print("Another one")
    
    }
  }
  
}

updateCoverages <- function(session,
                            gta.evaluation = "",
                            affected.flows = "",
                            implementers = "all",
                            keep.implementer = TRUE,
                            affected = "all",
                            keep.affected = TRUE,
                            keep.others = TRUE,
                            in.force.today = "Any",
                            announcement.period = c(NA,NA),
                            implementation.period = c(NA,NA),
                            keep.implementation.na = FALSE,
                            revocation.period = c(NA,NA),
                            keep.revocation.na = TRUE,
                            submission.period = c(NA,NA),
                            intervention.types = "all",
                            keep.type = TRUE,
                            mast.chapters = "all",
                            keep.mast = TRUE,
                            implementation.level = "any",
                            keep.level = TRUE,
                            eligible.firms = "any",
                            keep.firms = TRUE,
                            cpc.sectors = "all",
                            keep.cpc = TRUE,
                            hs.codes = "all",
                            keep.hs = TRUE,
                            intervention.ids = "",
                            keep.interventions = TRUE,
                            lag.adjustment = NA,
                            coverage.period = c(2009,year(Sys.Date())),
                            current.year.todate = T,
                            choose.tradebase = "base",
                            choose.output = "share",
                            adjust.for.intra = TRUE,
                            exporters = "all",
                            group.exporters = TRUE,
                            keep.exporters = TRUE,
                            incl.exporters.strictness = "ONEPLUS",
                            nr.exporters = "",
                            nr.exporters.incl = "ALL",
                            separate.exporter.groups = FALSE,
                            importers = "all",
                            group.importers = TRUE,
                            keep.importers = TRUE,
                            incl.importers.strictness = "ONEPLUS",
                            nr.importers = "",
                            nr.importers.incl = "ALL",
                            separate.importer.groups = FALSE,
                            implementer.role = c("Importer", "3rd country"),
                            nr.also.importers = "",
                            jointly.affected.importers = FALSE,
                            nr.also.exporters = "",
                            jointly.affected.exporters = FALSE,
                            hit.brackets = "",
                            group.type = TRUE,
                            group.mast = TRUE
) {
  
  
  updateSelectInput(session,
                    "gta.evaluation",
                    selected = gta.evaluation)
  
  updateSelectInput(session,
                    "affected.flows",
                    selected = affected.flows)
  
  updateMultiInput(session,
                   "implementers",
                   selected = implementers)
  
  updateCheckboxInput(session,
                      "keep.implementer",
                      value = keep.implementer)
  
  updateMultiInput(session,
                   "affected",
                   selected = affected)
  
  updateCheckboxInput(session,
                      "keep.affected",
                      value = keep.affected)
  
  updateCheckboxInput(session,
                      "keep.others",
                      value = keep.others)
  
  updateCheckboxInput(session,
                      "in.force.today",
                      value = in.force.today)
  
  updateDateRangeInput(session,
                       "announcement.period",
                       start = announcement.period[1],
                       end = announcement.period[2])
  
  updateDateRangeInput(session,
                       "implementation.period",
                       start = implementation.period[1],
                       end = implementation.period[2])
  
  updateDateRangeInput(session,
                       "revocation.period",
                       start = revocation.period[1],
                       end = revocation.period[2])
  
  
  updateDateRangeInput(session,
                       "submission.period",
                       start = submission.period[1],
                       end = submission.period[2])
  
  updateMultiInput(session,
                   "intervention.types",
                   selected = intervention.types)
  
  updateMultiInput(session,
                   "intervention.types",
                   selected = intervention.types)
  
  updateCheckboxInput(session,
                      "keep.type",
                      value = keep.type)
  
  updateMultiInput(session,
                   "mast.chapters",
                   selected = mast.chapters)
  
  updateCheckboxInput(session,
                      "keep.mast",
                      value = keep.mast)
  
  updateMultiInput(session,
                   "implementation.level",
                   selected = implementation.level)
  
  updateCheckboxInput(session,
                      "keep.level",
                      value = keep.level)
  
  updateMultiInput(session,
                   "eligible.firms",
                   selected = eligible.firms)
  
  updateCheckboxInput(session,
                      "keep.firms",
                      value = keep.firms)
  
  updateMultiInput(session,
                   "cpc.sectors",
                   selected = cpc.sectors)
  
  updateCheckboxInput(session,
                      "keep.cpc",
                      value = keep.cpc)
  
  updateMultiInput(session,
                   "hs.codes",
                   selected = hs.codes)
  
  
  updateCheckboxInput(session,
                      "keep.hs",
                      value = keep.hs)
  
  updateTextInput(session,
                  "intervention.ids",
                  value = intervention.ids)
  
  updateCheckboxInput(session,
                      "keep.interventions",
                      value = keep.interventions)
  
  updateDateInput(session,
                  "lag.adjustment",
                  value = lag.adjustment)

  updateSliderInput(session,
                    "coverage.period",
                    value = coverage.period)
  
  
  updateCheckboxInput(session,
                      "current.year.todate",
                      value = current.year.todate)
  
  
  updateSelectInput(session,
                    "choose.tradebase",
                    selected = choose.tradebase)
  
  updateSelectInput(session,
                    "choose.output",
                    selected = choose.output)
  
  updateCheckboxInput(session,
                      "adjust.for.intra",
                      value = adjust.for.intra)
  
  updateMultiInput(session,
                   "exporters",
                   selected = exporters)
  
  updateCheckboxInput(session,
                      "group.exporters",
                      value = group.exporters)
  
  
  updateCheckboxInput(session,
                      "keep.exporters",
                      value = keep.exporters)
  
  
  updateSelectInput(session,
                    "incl.exporters.strictness",
                    selected = incl.exporters.strictness)
  
  updateTextInput(session,
                  "nr.exporters",
                  value = nr.exporters)
  
  updateSelectInput(session,
                    "nr.exporters.incl",
                    selected = nr.exporters.incl)
  
  
  updateCheckboxInput(session,
                      "separate.exporter.groups",
                      value = separate.exporter.groups)
  
  
  updateMultiInput(session,
                   "importers",
                   selected = importers)
  
  updateCheckboxInput(session,
                      "group.importers",
                      value = group.importers)
  
  
  updateCheckboxInput(session,
                      "keep.importers",
                      value = keep.importers)
  
  updateSelectInput(session,
                    "incl.importers.strictness",
                    selected = incl.importers.strictness)
  
  updateTextInput(session,
                  "nr.importers",
                  value = nr.importers)
  
  updateSelectInput(session,
                    "nr.importers.incl",
                    selected = nr.importers.incl)
  
  
  updateCheckboxInput(session,
                      "separate.importer.groups",
                      value = separate.importer.groups)
  
  
  updateCheckboxGroupInput(session,
                           "implementer.role",
                           selected = implementer.role)
  
  updateNumericInput(session,
                     "nr.also.importers",
                     value = nr.also.importers)
  
  
  updateCheckboxInput(session,
                      "jointly.affected.importers",
                      value = jointly.affected.importers)
  
  updateNumericInput(session,
                     "nr.also.exporters",
                     value = nr.also.exporters)
  
  
  updateCheckboxInput(session,
                      "jointly.affected.exporters",
                      value = jointly.affected.exporters)
  
  updateTextInput(session,
                  "hit.brackets",
                  value = hit.brackets)
  
  
  updateCheckboxInput(session,
                      "group.type",
                      value = group.type)
  
  
  updateCheckboxInput(session,
                      "group.mast",
                      value = group.mast)
  
  
}
