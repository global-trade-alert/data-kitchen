q_dc1 <- function(session=session, type=NULL) {
  if(is.null(type)==F){
    title <- 'Initial Test Query'
    if(type=='title'){ return(title)}
    description <- 'Maybe deleted later'
    if(type=='description'){ return(description)}
  } else { 
    
 updateDataCounts(session = session,
    gta.evaluation.data.count = c('Red','Amber'),
    keep.implementation.na.data.count = TRUE,
    aggregate.y.data.count = 'intervention.id',
    group.cpc.data.count = TRUE,
    keep.revocation.na.data.count = TRUE,
    keep.affected.data.count = TRUE,
    keep.level.data.count = TRUE,
    affected.flows.data.count = 'Inward,Outward subsidy',
    hs.codes.data.count = c('all'),
    incl.affected.strictness.data.count = 'ONEPLUS',
    group.type.data.count = TRUE,
    keep.type.data.count = TRUE,
    implementation.period.data.count = c('2009-02-10','2018-06-12'),
    submission.period.data.count = c(NA,NA),
    lag.adjustment.data.count = '2019-08-13',
    intervention.ids.data.count = '',
    intervention.types.data.count = c('all'),
    implementation.level.data.count = c('any'),
    keep.firms.data.count = TRUE,
    group.implementer.data.count = TRUE,
    affected.data.count = c('all','G20'),
    group.mast.data.count = TRUE,
    keep.interventions.data.count = TRUE,
    eligible.firms.data.count = c('any'),
    keep.others.data.count = FALSE,
    in.force.today.data.count = 'Any',
    keep.mast.data.count = TRUE,
    cpc.sectors.data.count = c('all'),
    announcement.period.data.count = c(NA,NA),
    keep.cpc.data.count = TRUE,
    aggregate.x.data.count = c('year(date.implemented)','affected.sector','intervention.type','affected.jurisdiction'),
    revocation.period.data.count = c(NA,NA),
    implementers.data.count = c('United States of America'),
    group.affected.data.count = TRUE,
    keep.implementer.data.count = TRUE,
    nr.affected.incl.data.count = 'ALL',
    mast.chapters.data.count = c('B','D','E','F'),
    nr.affected.data.count = '',
    keep.hs.data.count = TRUE
)}}
