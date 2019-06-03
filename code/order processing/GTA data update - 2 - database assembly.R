
### gta_state_act
gta_state_act=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_measure.csv", sep=",")
setnames(gta_state_act, old="id", new="state_act_id")

## restricting to only the published ones (status==4)
gta_state_act=subset(gta_state_act, status_id==4)


gta_state_act_description=unique(gta_state_act[,c("state_act_id", "title", "description", "source")])
setnames(gta_state_act_description, old="description","announcement_description")
gta_state_act_description=subset(gta_state_act_description, state_act_id %in% gta_state_act$state_act_id)



# gta_state_act$title=NULL
gta_state_act$description=NULL
gta_state_act$source=NULL
gta_state_act$author_id=NULL
gta_state_act$status_id=NULL
gta_state_act$is_validated_after_migration=NULL
gta_state_act$submit_to_review_date=NULL
gta_state_act$publish_date=NULL
gta_state_act$reminder_date=NULL
gta_state_act$reminder_comment=NULL
gta_state_act$is_updated=NULL
gta_state_act$is_migrated=NULL
gta_state_act$last_update=NULL
gta_state_act$is_chunk_3_4=NULL
gta_state_act$raleated_measure=NULL

setnames(gta_state_act, old="evaluation_id","state_act_evaluation")
gta_state_act$state_act_evaluation=as.character(gta_state_act$state_act_evaluation)
gta_state_act$state_act_evaluation="Red"
gta_state_act$state_act_evaluation[gta_state_act$state_act_evaluation==2]="Amber"
gta_state_act$state_act_evaluation[gta_state_act$state_act_evaluation==3]="Green"


gta_state_act$announcement_date =as.Date.factor(gta_state_act$announcement_date, "%Y-%m-%d")
gta_state_act$creation_date =as.Date.factor(gta_state_act$creation_date, "%Y-%m-%d")


### gta_intervention
gta_intervention=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_intervention.csv", sep=",")
setnames(gta_intervention, old="id", new="intervention_id")
setnames(gta_intervention, old="measure_id", new="state_act_id")
setnames(gta_intervention, old="evaluation_id", new="gta_evaluation_id")
setnames(gta_intervention, old="is_fta_included", new="fta_exempt")
setnames(gta_intervention, old="unit", new="level_unit")
setnames(gta_intervention, old="intervention_area", new="commercial_area")
setnames(gta_intervention, old="is_chain_measure", new="high_frequency")
setnames(gta_intervention, old="measure_type_id", new="intervention_type_id")

gta_intervention=subset(gta_intervention, state_act_id %in% gta_state_act$state_act_id) # this ensures only published interventions are added

gta_intervention$measure_status_id=NULL
gta_intervention$subjective_score_editor=NULL
gta_intervention$subjective_score_reviewer_one=NULL
gta_intervention$subjective_score_reviewer_two=NULL
gta_intervention$old_id=NULL
gta_intervention$old_measure_id=NULL
gta_intervention$is_migrated=NULL
gta_intervention$investigation_status_id=NULL

gta_intervention$inception_date =as.Date.factor(gta_intervention$inception_date, "%Y-%m-%d")
gta_intervention$removal_date =as.Date.factor(gta_intervention$removal_date, "%Y-%m-%d")
gta_intervention$duration=0
gta_intervention$duration[is.na(gta_intervention$inception_date)==F]=Sys.Date()-gta_intervention$inception_date[is.na(gta_intervention$inception_date)==F]

gta_intervention_description=unique(gta_intervention[,c("intervention_id", "description")])
setnames(gta_intervention_description, old="description", "intervention_description")

gta_intervention$description=NULL

gta_intervention$gta_evaluation="Red"
gta_intervention$gta_evaluation[gta_intervention$gta_evaluation_id==2]="Amber"
gta_intervention$gta_evaluation[gta_intervention$gta_evaluation_id==3]="Green"
gta_intervention$gta_evaluation_id=NULL

gta_intervention$research_evaluation="Red"
gta_intervention$research_evaluation[gta_intervention$research_evaluation_id==2]="Amber"
gta_intervention$research_evaluation[gta_intervention$research_evaluation_id==3]="Green"
gta_intervention$research_evaluation_id=NULL

gta_intervention$is_duration_limited=NULL


gta_intervention$eligible_firms="all"
gta_intervention$eligible_firms[gta_intervention$eligible_firms_id==2]="SMEs"
gta_intervention$eligible_firms[gta_intervention$eligible_firms_id==3]="firm-specific"
gta_intervention$eligible_firms[gta_intervention$eligible_firms_id==4]="state-controlled"
gta_intervention$eligible_firms[gta_intervention$eligible_firms_id==5]="state trading enterprise"
gta_intervention$eligible_firms[gta_intervention$eligible_firms_id==6]="sector-specific"
gta_intervention$eligible_firms_id=NULL


gta_intervention$implementation_level="supranational"
gta_intervention$implementation_level[gta_intervention$implementation_level_id==2]="national"
gta_intervention$implementation_level[gta_intervention$implementation_level_id==3]="subnational"
gta_intervention$implementation_level[gta_intervention$implementation_level_id==4]="SEZ"
gta_intervention$implementation_level[gta_intervention$implementation_level_id==5]="IFI"
gta_intervention$implementation_level[gta_intervention$implementation_level_id==6]="NFI"
gta_intervention$implementation_level_id=NULL

gta_intervention$commercial_area=as.character(gta_intervention$commercial_area)
gta_intervention$commercial_area[gta_intervention$commercial_area==1]="goods"
gta_intervention$commercial_area[gta_intervention$commercial_area==2]="service"
gta_intervention$commercial_area[gta_intervention$commercial_area==3]="investment"
gta_intervention$commercial_area[gta_intervention$commercial_area==4]="migration"

gta_intervention$affected_flow="inward"
gta_intervention$affected_flow[gta_intervention$affected_flow_id==2]="outward"
gta_intervention$affected_flow[gta_intervention$affected_flow_id==3]="outward subsidy"
gta_intervention$affected_flow_id=NULL

gta_intervention$migration_id=NULL
gta_intervention$to_delete=NULL
gta_intervention$is_chunk_3_4 =NULL
gta_intervention$is_from_duplicates =NULL
gta_intervention$to_delete_new =NULL
gta_intervention$to_recalculate =NULL
gta_intervention$aj_freeze =NULL
gta_intervention$dm_freeze =NULL


## gta_measure_type
gta_measure_type=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_measure_type.csv", sep=",")
setnames(gta_measure_type, old="id", new="intervention_type_id")
setnames(gta_measure_type, old="name", new="intervention_type")

gta_intervention=merge(gta_intervention, gta_measure_type[,c("intervention_type_id", "intervention_type")], by="intervention_type_id", all.x=T)
gta_intervention$intervention_type_id=NULL
rm(gta_measure_type)


## gta_implementing_jurisdiction // affected // distorted
gta_jurisdiction=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_jurisdiction.csv", sep=",")
setnames(gta_jurisdiction, old="id", new="jurisdiction_id")

gta_implementing_jurisdiction=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_implementing_jurisdiction.csv", sep=",")
gta_implementing_jurisdiction=merge(gta_implementing_jurisdiction, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id")
gta_implementing_jurisdiction$jurisdiction_id=NULL
setnames(gta_implementing_jurisdiction, old="name", "implementing_jurisdiction")
setnames(gta_implementing_jurisdiction, old="un_code", "un_code_implementer")
gta_implementing_jurisdiction=subset(gta_implementing_jurisdiction, intervention_id %in% gta_intervention$intervention_id)


gta_distorted_market=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_distorted_market.csv", sep=",")
gta_distorted_market=merge(gta_distorted_market, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id")
gta_distorted_market$jurisdiction_id=NULL
setnames(gta_distorted_market, old="name", "distorted_market")
setnames(gta_distorted_market, old="type", "type_distorted")
setnames(gta_distorted_market, old="un_code", "un_code_distorted")
gta_distorted_market=subset(gta_distorted_market, intervention_id %in% gta_intervention$intervention_id)


gta_affected_jurisdiction=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_affected_jurisdiction.csv", sep=",")
gta_affected_jurisdiction=merge(gta_affected_jurisdiction, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id")
gta_affected_jurisdiction$jurisdiction_id=NULL
setnames(gta_affected_jurisdiction, old="name", "affected_jurisdiction")
setnames(gta_affected_jurisdiction, old="type", "type_affected")
setnames(gta_affected_jurisdiction, old="un_code", "un_code_affected")
gta_affected_jurisdiction=subset(gta_affected_jurisdiction, intervention_id %in% gta_intervention$intervention_id)



### gta_affected_tariff_line
gta_affected_tariff_line=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_affected_tariff_line.csv", sep=",")
gta_affected_tariff_line=subset(gta_affected_tariff_line, intervention_id %in% gta_intervention$intervention_id)


### gta_affected_sector
gta_affected_sector=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_affected_sector.csv", sep=",")
gta_affected_sector$type=NULL
gta_affected_sector=subset(gta_affected_sector, intervention_id %in% gta_intervention$intervention_id)

## gta_it_revised
gta_tuple=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_it_revised.csv", sep=",")
gta_tuple=subset(gta_tuple, intervention_id %in% gta_intervention$intervention_id)

setnames(gta_tuple, old="sector_code_3", "sector_code")

gta_tuple$ariff_line_id=NULL
gta_tuple$value=NULL

## adding implementer names & UN codes
setnames(gta_tuple, old="implementing_jurisdiction_id", "jurisdiction_id")
gta_tuple=merge(gta_tuple, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id", all.x=T)

# should be 0
nrow(subset(gta_tuple, is.na(name)==T))

setnames(gta_tuple, old="name", "implementing_jurisdiction")
setnames(gta_tuple, old="un_code", "un_code_implementer")
gta_tuple$jurisdiction_id=NULL

## adding distorted names & UN codes
setnames(gta_tuple, old="distorted_market_id", "jurisdiction_id")
gta_tuple=merge(gta_tuple, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id", all.x=T)

# should be 0
nrow(subset(gta_tuple, is.na(name)==T))

setnames(gta_tuple, old="name", "distorted_market")
setnames(gta_tuple, old="un_code", "un_code_distorted")
gta_tuple$jurisdiction_id=NULL


## adding affected names & UN codes
setnames(gta_tuple, old="affected_jurisdiction_id", "jurisdiction_id")
gta_tuple=merge(gta_tuple, gta_jurisdiction[,c("jurisdiction_id", "name", "un_code")], by="jurisdiction_id", all.x=T)

# should be 0
nrow(subset(gta_tuple, is.na(name)==T))

setnames(gta_tuple, old="name", "affected_jurisdiction")
setnames(gta_tuple, old="un_code", "un_code_affected")
gta_tuple$jurisdiction_id=NULL

## gta_tariff_line
gta_tariff_line=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_tariff_line.csv", sep=",")
setnames(gta_tariff_line, old="id", new="tariff_line_id")
setnames(gta_tariff_line, old="code", new="affected_products")

gta_tuple=merge(gta_tuple, gta_tariff_line[,c("tariff_line_id", "affected_products")], by="tariff_line_id", all.x=T)
gta_tuple$tariff_line_id=NULL

## gta_affected_tariff_line
gta_affected_tariff_line=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_affected_tariff_line.csv", sep=",")
setnames(gta_affected_tariff_line, old="tariff_line_code", "affected_products")


## PL: added gta_jurisdiction, gta_jurisdiction_group, and gta_jurisdiction_group_member
gta_jurisdiction=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_jurisdiction.csv", sep=",")
gta_jurisdiction_group=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_jurisdiction_group.csv", sep=",")
gta_jurisdiction_group_member=read.csv("/home/rstudio/Dropbox/GTA cloud/data/database replica/gta_jurisdiction_group_member.csv", sep=",")

rm(gta_tariff_line)
t=paste(Sys.Date(), "_", hour(Sys.time()), minute(Sys.time()), sep="")

save.image(file='/home/rstudio/Dropbox/GTA cloud/data/database replica/database replica - parts - base.Rdata')
eval(parse(text=paste("save.image(file='/home/rstudio/Dropbox/GTA cloud/data/database replica/database replica - parts - ",t,".Rdata')", sep="")))  


