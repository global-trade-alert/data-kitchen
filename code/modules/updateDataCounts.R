callUpdateDataCounts <- function(session,
                                 query = NULL, 
                                 path = NULL) {
    
    if(is.null(path)) {
      stop("Please specify path")
    }
    
  for(fct in list.files(paste0(path,"/code/modules/queries/datacounts"), pattern = ".R", full.names=T)){
    source(fct)
  }
  
  if (is.null(query)==F) {
    
    if (query == "Unset") {
      
      updateDataCounts(session = session)
    
      } else {
    
      updateDataCounts(session = session)
        
      eval(parse(text=paste0(query,"(session = session)")))
        
      
    }
    
  }
  
}

updateDataCounts <- function(session,
                             gta.evaluation.data.count = c("Red", "Amber", "Green"),
                             affected.flows.data.count = c("Inward","Outward","Outward subsidy"),
                             implementers.data.count = "all",
                             keep.implementer.data.count = TRUE,
                             group.implementer.data.count = TRUE,
                             affected.data.count = "all",
                             keep.affected.data.count = TRUE,
                             group.affected.data.count = TRUE,
                             keep.others.data.count = FALSE,
                             incl.affected.strictness.data.count = "ONEPLUS",
                             nr.affected.data.count = "",
                             nr.affected.incl.data.count = "ALL",
                             in.force.today.data.count = "Any",
                             announcement.period.data.count = c(NA,NA),
                             implementation.period.data.count = c(NA,NA),
                             keep.implementation.na.data.count = TRUE,
                             revocation.period.data.count = c(NA,NA),
                             keep.revocation.na.data.count = TRUE,
                             submission.period.data.count = c(NA,NA),
                             intervention.types.data.count = "all",
                             keep.type.data.count = TRUE,
                             group.type.data.count = TRUE,
                             mast.chapters.data.count = "all",
                             keep.mast.data.count = TRUE,
                             group.mast.data.count = TRUE,
                             implementation.level.data.count = "any",
                             keep.level.data.count = TRUE,
                             eligible.firms.data.count = "any",
                             keep.firms.data.count = TRUE,
                             cpc.sectors.data.count = "all",
                             keep.cpc.data.count = TRUE,
                             group.cpc.data.count = TRUE,
                             all.service.cpc.data.count = TRUE,
                             hs.codes.data.count = "all",
                             keep.hs.data.count = TRUE,
                             intervention.ids.data.count = "",
                             keep.interventions.data.count = TRUE,
                             lag.adjustment.data.count = NA,
                             aggregate.y.data.count = NA,
                             aggregate.x.data.count = c("Implemented (year)" = "year(date.implemented)"),
                             xlsx.data.count = TRUE,
                             interventions.list.data.count = FALSE,
                             line.data.count = FALSE,
                             bar.data.count = FALSE,
                             map.data.count = FALSE,
                             tile.data.count = FALSE,
                             line.title.data.count = "",
                             line.legend.title.data.count = "",
                             line.x.axis.title.data.count = "",
                             line.y.axis.title.data.count = "",
                             line.colour.data.count = "gta_colour$qualitative",
                             line.y.var.data.count = "",
                             line.group.var.data.count = "",
                             bar.title.data.count = "",
                             bar.legend.title.data.count = "",
                             bar.x.axis.title.data.count = "",
                             bar.y.axis.title.data.count = "",
                             bar.colour.data.count = "gta_colour$qualitative",
                             bar.y.var.data.count = "",
                             bar.group.var.data.count = "",
                             map.title.data.count = "",
                             map.legend.title.data.count = "",
                             map.colour.low.data.count = "gta_colour$green[1]",
                             map.colour.high.data.count = "gta_colour$red[1]",
                             map.countries.data.count = "",
                             map.value.data.count = "",
                             map.splits.data.count = 3,
                             map.brackets.data.count = "",
                             tile.title.data.count = "",
                             tile.legend.title.data.count = "",
                             tile.x.axis.title.data.count = "",
                             tile.y.axis.title.data.count = "",
                             tile.colour.low.data.count = "gta_colour$green[1]",
                             tile.colour.high.data.count = "gta_colour$red[1]",
                             tile.x.var.data.count = "",
                             tile.y.var.data.count = "",
                             tile.value.var.data.count = "")  {
  
  updateSelectInput(session,
                    "aggregate.y.data.count",
                    selected = aggregate.y.data.count)
  
  updateSelectInput(session,
                    "aggregate.x.data.count",
                    selected = aggregate.x.data.count)
  
  updateSelectInput(session,
                    "gta.evaluation.data.count",
                    selected = gta.evaluation.data.count)
  
  
  updateSelectInput(session,
                    "affected.flows.data.count",
                    selected = affected.flows.data.count)
  
  
  updateMultiInput(session,
                   "implementers.data.count",
                   selected = implementers.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.implementer.data.count",
                      value = keep.implementer.data.count)
  
  updateCheckboxInput(session,
                      "group.implementer.data.count",
                      value = group.implementer.data.count)
  
  
  updateMultiInput(session,
                   "affected.data.count",
                   selected = affected.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.affected.data.count",
                      value = keep.affected.data.count)
  
  
  updateCheckboxInput(session,
                      "group.affected.data.count",
                      value = group.affected.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.others.data.count",
                      value = keep.others.data.count)
  
  updateSelectInput(session,
                    "incl.affected.strictness.data.count",
                    selected = incl.affected.strictness.data.count)
  
  updateTextInput(session,
                  "nr.affected.data.count",
                  value = nr.affected.data.count)
  
  updateSelectInput(session,
                    "nr.affected.incl.data.count",
                    selected = nr.affected.incl.data.count)
  
  
  updateCheckboxInput(session,
                      "in.force.today.data.count",
                      value = in.force.today.data.count)
  
  
  updateCheckboxInput(session,
                      "in.force.today.data.count",
                      value = in.force.today.data.count)
  
  
  updateDateRangeInput(session,
                       "announcement.period.data.count",
                       start = announcement.period.data.count[1],
                       end = announcement.period.data.count[2])
  
  
  updateDateRangeInput(session,
                       "implementation.period.data.count",
                       start = implementation.period.data.count[1],
                       end = implementation.period.data.count[2])
  
  
  updateDateRangeInput(session,
                       "revocation.period.data.count",
                       start = revocation.period.data.count[1],
                       end = revocation.period.data.count[2])
  
  
  
  updateDateRangeInput(session,
                       "submission.period.data.count",
                       start = submission.period.data.count[1],
                       end = submission.period.data.count[2])
  
  
  updateMultiInput(session,
                   "intervention.types.data.count",
                   selected = intervention.types.data.count)
  
  
  updateMultiInput(session,
                   "intervention.types.data.count",
                   selected = intervention.types.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.type.data.count",
                      value = keep.type.data.count)
  
  
  updateCheckboxInput(session,
                      "group.type.data.count",
                      value = group.type.data.count)
  
  
  updateMultiInput(session,
                   "mast.chapters.data.count",
                   selected = mast.chapters.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.mast.data.count",
                      value = keep.mast.data.count)
  
  
  updateCheckboxInput(session,
                      "group.mast.data.count",
                      value = group.mast.data.count)
  
  
  updateMultiInput(session,
                   "implementation.level.data.count",
                   selected = implementation.level.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.level.data.count",
                      value = keep.level.data.count)
  
  
  updateMultiInput(session,
                   "eligible.firms.data.count",
                   selected = eligible.firms.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.firms.data.count",
                      value = keep.firms.data.count)
  
  
  updateMultiInput(session,
                   "cpc.sectors.data.count",
                   selected = cpc.sectors.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.cpc.data.count",
                      value = keep.cpc.data.count)
  
  updateCheckboxInput(session,
                      "group.cpc.data.count",
                      value = group.cpc.data.count)
  
  updateCheckboxInput(session,
                      "all.service.cpc.data.count",
                      value = all.service.cpc.data.count)
  
  
  updateMultiInput(session,
                   "hs.codes.data.count",
                   selected = hs.codes.data.count)
  
  
  
  updateCheckboxInput(session,
                      "keep.hs.data.count",
                      value = keep.hs.data.count)
  
  
  updateTextInput(session,
                  "intervention.ids.data.count",
                  value = intervention.ids.data.count)
  
  
  updateCheckboxInput(session,
                      "keep.interventions.data.count",
                      value = keep.interventions.data.count)
  
  
  updateDateInput(session,
                  "lag.adjustment.data.count",
                  value = lag.adjustment.data.count)


# PLOT VALUES
updateCheckboxInput(session,
                    "xlsx.data.count",
                    value = xlsx.data.count)
updateCheckboxInput(session,
                    "interventions.list.data.count",
                    value = interventions.list.data.count)
updateCheckboxInput(session,
                    "line.data.count",
                    value = line.data.count)
updateCheckboxInput(session,
                    "bar.data.count",
                    value = bar.data.count)
updateCheckboxInput(session,
                    "map.data.count",
                    value = map.data.count)
updateCheckboxInput(session,
                    "tile.data.count",
                    value = tile.data.count)

# LINE
updateTextInput(session,
                "line.title.data.count",
                value = line.title.data.count)
updateTextInput(session,
                "line.legend.title.data.count",
                value = line.legend.title.data.count)
updateTextInput(session,
                "line.x.axis.title.data.count",
                value = line.x.axis.title.data.count)
updateTextInput(session,
                "line.y.axis.title.data.count",
                value = line.y.axis.title.data.count)
updateSelectInput(session,
                  "line.colour.data.count",
                  selected = line.colour.data.count)
updateSelectInput(session,
                  "line.y.var.data.count",
                  selected = line.y.var.data.count)
updateSelectInput(session,
                  "line.group.var.data.count",
                  selected = line.group.var.data.count)

# BAR
updateTextInput(session,
                "bar.title.data.count",
                value = bar.title.data.count)
updateTextInput(session,
                "bar.legend.title.data.count",
                value = bar.legend.title.data.count)
updateTextInput(session,
                "bar.x.axis.title.data.count",
                value = bar.x.axis.title.data.count)
updateTextInput(session,
                "bar.y.axis.title.data.count",
                value = bar.y.axis.title.data.count)
updateSelectInput(session,
                  "bar.colour.data.count",
                  selected = bar.colour.data.count)
updateSelectInput(session,
                  "bar.y.var.data.count",
                  selected = bar.y.var.data.count)
updateSelectInput(session,
                  "bar.group.var.data.count",
                  selected = bar.group.var.data.count)

# MAP
updateTextInput(session,
                "map.title.data.count",
                value = map.title.data.count)
updateTextInput(session,
                "map.legend.title.data.count",
                value = map.legend.title.data.count)
updateSelectInput(session,
                  "map.colour.low.data.count",
                  selected = map.colour.low.data.count)
updateSelectInput(session,
                  "map.colour.high.data.count",
                  selected = map.colour.high.data.count)
updateSelectInput(session,
                  "map.countries.data.count",
                  selected = map.countries.data.count)
updateSelectInput(session,
                  "map.value.data.count",
                  selected = map.value.data.count)
updateNumericInput(session,
                   "map.splits.data.count",
                   value = map.splits.data.count)
updateTextInput(session,
                "map.brackets.data.count",
                value = map.brackets.data.count)

# TILE
updateTextInput(session,
                "tile.title.data.count",
                value = tile.title.data.count)
updateTextInput(session,
                "tile.legend.title.data.count",
                value = tile.legend.title.data.count)
updateTextInput(session,
                "tile.x.axis.title.data.count",
                value = tile.x.axis.title.data.count)
updateTextInput(session,
                "tile.y.axis.title.data.count",
                value = tile.y.axis.title.data.count)
updateSelectInput(session,
                  "tile.colour.low.data.count",
                  selected = tile.colour.low.data.count)
updateSelectInput(session,
                  "tile.colour.high.data.count",
                  selected = tile.colour.high.data.count)
updateSelectInput(session,
                  "tile.x.var.data.count",
                  selected = tile.x.var.data.count)
updateSelectInput(session,
                  "tile.y.var.data.count",
                  selected = tile.y.var.data.count)
updateSelectInput(session,
                  "tile.value.var.data.count",
                  selected = tile.value.var.data.count)


}

