country.iso <- read.csv("/home/rstudio/Dropbox/GTA cloud/R help files/country_iso_un.csv", sep=";")

# Compiling new master data
load("/home/rstudio/Dropbox/GTA cloud/data/database replica/database replica - parts - base.Rdata")

gta_intervention=merge(gta_intervention, gta_state_act[,c("state_act_id", "creation_date", "title", "announcement_date")], by="state_act_id", all.x=T)

#lines used for master file: Date.of.inception,Gta.Evaluation, i.un, Measure.Type, Published.at, intervention.id,
master=gta_intervention[,c("intervention_id", "state_act_id", "title", "announcement_date","inception_date","removal_date","intervention_type", "gta_evaluation","implementation_level","eligible_firms","affected_flow" ,"creation_date")]
master=merge(master, gta_implementing_jurisdiction[,c("intervention_id","implementing_jurisdiction", "un_code_implementer")], by="intervention_id", all.x=T)

setnames(master, old="inception_date", "date.implemented")
setnames(master, old="removal_date", "date.removed")
setnames(master, old="announcement_date", "date.announced")
setnames(master, old="gta_evaluation", "gta.evaluation")
setnames(master, old="un_code_implementer", "i.un")
setnames(master, old="intervention_type", "intervention.type")
setnames(master, old="creation_date", "date.published")
setnames(master, old="intervention_id", "intervention.id")
setnames(master, old="state_act_id", "state.act.id")
setnames(master, old="implementing_jurisdiction", "implementing.jurisdiction")
setnames(master, old="title", "title")
setnames(master, old="implementation_level", "implementation.level")
setnames(master, old="affected_flow", "affected.flow")
setnames(master, old="eligible_firms", "eligible.firms")


today=Sys.Date()
master$currently.in.force="No"
master$currently.in.force[master$date.implemented<=today & (master$date.removed>today | is.na(master$date.removed)==T)]="Yes"

## sectors, products, AJ
## where we have support tables, we have it in gta_tuple  
master.tuple=subset(master, intervention.id %in% gta_tuple$intervention_id)
master.else=subset(master,! intervention.id %in% gta_tuple$intervention_id)

## correcting for zeroes at the start of gta_tuple
gta_tuple$l.hs=nchar(gta_tuple$affected_products)
gta_tuple$l.cpc=nchar(gta_tuple$sector_code)

gta_tuple$affected_products=as.character(gta_tuple$affected_products)
gta_tuple$sector_code=as.character(gta_tuple$sector_code)

gta_tuple$sector_code[gta_tuple$l.cpc==2 & is.na(gta_tuple$l.cpc)==F]=paste("0", gta_tuple$sector_code[gta_tuple$l.cpc==2 & is.na(gta_tuple$l.cpc)==F], sep="")
gta_tuple$affected_products[gta_tuple$l.hs==5 & is.na(gta_tuple$l.hs)==F]=paste("0", gta_tuple$affected_products[gta_tuple$l.hs==5  & is.na(gta_tuple$l.hs)==F], sep="")

gta_tuple$l.hs=NULL
gta_tuple$l.cpc=NULL


## the rest goes through the other gta_ tables. 
## First line creates a table of all combinations, 2nd adds sectors (where available), and last products (where available).
# all=unique(gta_tuple[,c("intervention_id","un_code_implementer","un_code_affected")])
all=gta_tuple[,c("intervention_id","un_code_implementer","un_code_affected")]
all$x=1
all=aggregate(x ~intervention_id + un_code_implementer  + un_code_affected, all, sum)
all$x=NULL

cpc.hs=merge(aggregate(sector_code ~ intervention_id + un_code_implementer + un_code_affected, gta_tuple, function(x) paste(unique(x), collapse=", ")),
             aggregate(affected_products ~ intervention_id + un_code_implementer + un_code_affected, gta_tuple, function(x) paste(unique(x), collapse=", ")), 
             by=c("intervention_id","un_code_implementer","un_code_affected"), all.x=T)

cpc.hs=merge(all, cpc.hs, by=c("intervention_id","un_code_implementer","un_code_affected"), all.x=T)

## should be =1
length(unique(cpc.hs$intervention_id))/length(unique(master.tuple$intervention.id))

setnames(cpc.hs, old="affected_products", "affected.product")
setnames(cpc.hs, old="sector_code", "affected.sector")
setnames(cpc.hs, old="un_code_affected", "a.un")
setnames(cpc.hs, old="un_code_implementer", "i.un")
setnames(cpc.hs, old="intervention_id", "intervention.id")

master.tuple=merge(master.tuple, cpc.hs, by=c("intervention.id","i.un"), all.x=T)

## the non-tuple cases
setnames(gta_affected_sector,old="intervention_id","intervention.id")
setnames(gta_affected_sector,old="sector_code","affected.sector")

setnames(gta_affected_tariff_line,old="intervention_id","intervention.id")
setnames(gta_affected_tariff_line,old="affected_products","affected.product")

gta_affected_jurisdiction=subset(gta_affected_jurisdiction, type_affected!="D")
setnames(gta_affected_jurisdiction, old="intervention_id", "intervention.id")
setnames(gta_affected_jurisdiction, old="un_code_affected", "a.un")
setnames(gta_affected_jurisdiction, old="affected_jurisdiction", "affected.jurisdiction")
gta_affected_jurisdiction$type_affected=NULL


## correcting for zeroes at the start of gta_tuple
gta_affected_tariff_line$l.hs=nchar(gta_affected_tariff_line$affected.product)
gta_affected_tariff_line$affected.product=as.character(gta_affected_tariff_line$affected.product)
gta_affected_tariff_line$affected.product[gta_affected_tariff_line$l.hs==5 & is.na(gta_affected_tariff_line$l.hs)==F]=paste("0", gta_affected_tariff_line$affected.product[gta_affected_tariff_line$l.hs==5  & is.na(gta_affected_tariff_line$l.hs)==F], sep="")
gta_affected_tariff_line$l.hs=NULL


gta_affected_sector$l.cpc=nchar(gta_affected_sector$affected.sector)
gta_affected_sector$affected.sector=as.character(gta_affected_sector$affected.sector)
gta_affected_sector$affected.sector[gta_affected_sector$l.cpc==2 & is.na(gta_affected_sector$l.cpc)==F]=paste("0", gta_affected_sector$affected.sector[gta_affected_sector$l.cpc==2 & is.na(gta_affected_sector$l.cpc)==F], sep="")
gta_affected_sector$l.cpc=NULL


master.else=merge(master.else,
                  aggregate(affected.sector ~ intervention.id, gta_affected_sector, function(x) paste(unique(x), collapse=", ")),
                  by="intervention.id", all.x=T)

master.else=merge(master.else,
                  aggregate(affected.product ~ intervention.id, gta_affected_tariff_line, function(x) paste(unique(x), collapse=", ")),
                  by="intervention.id", all.x=T)

master.else=merge(master.else,
                  gta_affected_jurisdiction,
                  by="intervention.id", all.x=T)

master.tuple=merge(master.tuple, unique(gta_affected_jurisdiction[,c("a.un","affected.jurisdiction")]), by="a.un", all.x=T)
length(unique(rbind(master.tuple, master.else)$intervention.id))/length(unique(master$intervention.id))

master=rbind(master.tuple, master.else)

## checking that sector and HS codes have 3 or 6 digits




## adding i.atleastone.G20
master$i.atleastone.G20=as.numeric(master$intervention.id %in% subset(master, i.un %in% subset(country.iso, G20==1)$UN)$intervention.id )

master$a.atleastone.G20=as.numeric(master$intervention.id %in% subset(master, a.un %in% subset(country.iso, G20==1)$UN)$intervention.id )

## adding MAST chapters
mt=read.csv("/home/rstudio/Dropbox/GTA cloud/R help files/gta_measure_type.csv")
mt$mast_chapter=as.character(mt$mast_chapter)
mt$mast_chapter[mt$mast_chapter=="TAU"]="TARIFF"
setnames(mt, old="name", "intervention.type")

mast=read.csv("/home/rstudio/Dropbox/GTA cloud/R help files/MAST chapter labels.csv", sep=";")
setnames(mast, old="subchapter.label", "mast.chapter")
setnames(mast, old="mast.subchapter", "mast_chapter")

mt=merge(mt, unique(mast[,c("mast_chapter", "mast.chapter")]), by="mast_chapter", all.x=T)

setnames(mt, old="mast_chapter","mast.id")

master$intervention.type=as.character(master$intervention.type)
mt$intervention.type=as.character(mt$intervention.type)

master=merge(master, mt[,c("intervention.type","mast.id", "mast.chapter")], by="intervention.type", all.x=T)
unique(master$intervention.type[is.na(master$mast.chapter)==T])

save(master, file="/home/rstudio/Dropbox/GTA cloud/data/master_plus.Rdata")
