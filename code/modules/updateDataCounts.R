callUpdateDataCounts <- function(session,
                                 query = NULL) {


  if (is.null(query)==F) {

    if (query == "Unset") {

      updateDataCounts(session = session)
    }

    if (query == "query_1") {

      updateDataCounts(session = session)
      updateDataCounts(session = session,
                       gta.evaluation.data.count = c("Red","Amber"),
                       implementers.data.count = c("United States of America","China"),
                       affected.data.count = c("G20"))
    }

  }

}



updateDataCounts <- function(session,
                             gta.evaluation.data.count = "",
                             affected.flows.data.count = "",
                             implementers.data.count = "",
                             keep.implementer.data.count = TRUE,
                             group.implementer.data.count = TRUE,
                             affected.data.count = "",
                             keep.affected.data.count = TRUE,
                             group.affected.data.count = TRUE,
                             keep.others.data.count = TRUE,
                             incl.affected.strictness.data.count = "ONEPLUS",
                             nr.affected.data.count = "1,999",
                             nr.affected.incl.data.count = "ALL",
                             in.force.today.data.count = "Any",
                             announcement.period.data.count = c(NA,NA),
                             implementation.period.data.count = c(NA,NA),
                             keep.implementation.na.data.count = TRUE,
                             revocation.period.data.count = c(NA,NA),
                             keep.revocation.na.data.count = TRUE,
                             submission.period.data.count = c(NA,NA),
                             intervention.types.data.count = "",
                             keep.type.data.count = TRUE,
                             group.type.data.count = TRUE,
                             mast.chapters.data.count = "",
                             keep.mast.data.count = TRUE,
                             group.mast.data.count = TRUE,
                             implementation.level.data.count = "any",
                             keep.level.data.count = TRUE,
                             eligible.firms.data.count = "any",
                             keep.firms.data.count = TRUE,
                             cpc.sectors.data.count = "",
                             keep.cpc.data.count = TRUE,
                             group.cpc.data.count = TRUE,
                             all.service.cpc.data.count = TRUE,
                             hs.codes.data.count = "",
                             keep.hs.data.count = TRUE,
                             intervention.ids.data.count = "",
                             keep.interventions.data.count = TRUE,
                             lag.adjustment.data.count = NA) {


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
                      value = incl.affected.strictness.data.count)
    
    updateTextInput(session,
                    "nr.affected.data.count",
                    value = nr.affected.data.count)
    
    updateSelectInput(session,
                      "nr.affected.incl.data.count",
                      value = nr.affected.incl.data.count)

    
    updateNumericInput(session,
                       "affected.also.nr.data.count",
                       value = affected.also.nr.data.count)


    updateCheckboxInput(session,
                        "affected.jointly.data.count",
                        value = affected.jointly.data.count)


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



}
