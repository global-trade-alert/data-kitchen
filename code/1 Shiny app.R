require(shiny)
library(openxlsx)
library(DT)
library(shinyWidgets)
library(gtalibrary)
library(data.table)
library(mailR)
library(tidyverse)

# gta_update_library()


rm(list = ls())

setwd("/home/rstudio/Dropbox/GTA cloud")
# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
path = "17 Shiny/4 data kitchen/"
# path = "0 dev/data-kitchen-pb/"

# Load required data
countries <- gtalibrary::country.correspondence
intervention_types <- gtalibrary::int.mast.types
sectors <- gtalibrary::cpc.names


## create list of sector names & codes
sec.digit=sectors$cpc.digit.level
sec.cpc=sectors$cpc
sec.out=character(nrow(sectors))
for(i in 1:length(sec.digit)){
  if(sec.digit[i]==2){
    sec.out[i]=sprintf("%02i",sec.cpc[i])
  }  else{
    sec.out[i]=sprintf("%03i",sec.cpc[i])
  }
}
sectors$cpc=sec.out
rm(sec.digit, sec.cpc, sec.out)
sectors$merged <- strtrim(paste(sectors$cpc, sectors$cpc.name, sep = " - "),40)
sectors$merged=gsub("'","",sectors$merged)
sectors$cpc.3digit=apply(sectors,1, function(x) paste(sectors$cpc[grepl(paste("^", x[1],sep=""), sectors$cpc) & sectors$cpc.digit.level==3], collapse=","))
eval(parse(text=paste("sectors_list=c(",paste(paste("'", sectors$merged,"'='",sectors$cpc.3digit,"'",sep=""), collapse=","),")")))

# create list of HS names and codes
products <- gtalibrary::hs.names
products$hs.name=as.character(products$hs.name)
products$merged <- substr(paste(sprintf("%06i",as.numeric(products$HS12code)), products$hs.name, sep = " - "),1,40)
products$merged=gsub("'","",products$merged)
product.groups <- gtalibrary::hs.codes
product.groups <- names(product.groups)
product.groups <- product.groups[grepl("is.", product.groups)]
product.groups <- gsub("is.","",product.groups)
product.groups <- gsub("\\."," ",product.groups)
product.groups <- tools::toTitleCase(product.groups)
# product.groups<- c('Raw materials','Intermediate goods','Consumer goods','Capital goods','Agricultural goods')

eval(parse(text=paste("products_list=c(",paste(paste("'",product.groups,"'", sep=""), collapse = ","),",",paste(paste("'", products$merged,"'='",products$HS12code,"'",sep=""), collapse=","),")")))

colour.list <- c("Red" = "gta_colour$red[1]",
                 "Red light" = "gta_colour$red[4]",
                 "Green" = "gta_colour$green[1]",
                 "Green light" = "gta_colour$green[4]",
                 "Amber" = "gta_colour$amber[1]",
                 "Amber light" = "gta_colour$amber[4]",
                 "Blue" = "gta_colour$blue[1]",
                 "Blue light" = "gta_colour$blue[4]",
                 "Brown" = "gta_colour$brown[1]",
                 "Brown light" = "gta_colour$brown[4]",
                 "Desert" = "gta_colour$desert[1]",
                 "Desert light" = "gta_colour$desert[4]",
                 "Turquoise" = "gta_colour$turquoise[1]",
                 "Turquoise light" = "gta_colour$turquoise[4]",
                 "Grey" = "gta_colour$grey[1]",
                 "Grey light" = "gta_colour$grey[4]")

column.list <- c("Intervention IDs" = "intervention.id",
                 "State Act IDs" = "state.act.id",
                 "GTA Evaluation (Red, Amber, Green)" = "gta.evaluation",
                 "GTA Evaluation (Harmful, Liberalising)" = "gta.evaluation.harmful.liberalising",
                 "Affected Jurisdiction" = "affected.jurisdiction",
                 "Implementing Jurisdiction" = "implementing.jurisdiction",
                 "Intervention Type" = "intervention.type",
                 "Affected Products" = "affected.product",
                 "Affected Sectors" = "affected.sector",
                 "Mast Chapter" = "mast.chapter",
                 "Implementation Level" = "implementation.level",
                 "Implemented (year)" = "year(date.implemented)",
                 "Implemented (month)" = "month(date.implemented)",
                 "Announced (year)" = "year(date.announced)",
                 "Announced (month)" = "month(date.announced)",
                 "Revoked (year)" = "year(date.removed)",
                 "Revoked (month)" = "month(date.removed)",
                 "Published (year)" = "year(date.published)",
                 "Published (month)" = "month(date.published)",
                 "At least on implementer G20" = "i.atleastone.G20",
                 "At least on affected G20" = "a.atleastone.G20",
                 "Currently in Force" = "currently.in.force",
                 "Eligible Firms" = "eligible.firms")

column.list.numeric <- c("Intervention IDs" = "intervention.id",
                         "State Act IDs" = "state.act.id",
                         "Affected Products" = "affected.product",
                         "Affected Sectors" = "affected.sector")

mast_chapters = c( "A: Sanitary and phytosanitary measure" = "A",
                   "B: Technical barriers to trade" = "B",
                   "D: Contingent trade-protective measures" = "D",
                   "E: Non-automatic licensing, quotas etc." = "E",
                   "F: Price-control measures, including additional taxes and charges" = "F",
                   "G: Finance measures" = "G",
                   "I: Trade-related investment measures" = "I",
                   "L: Subsidies (excl. export subsidies)" = "L",
                   "M: Government procurement restrictions" = "M",
                   "N: Intellectual Property" = "N",
                   "P: Export-related measures (incl. subsidies)" = "P",
                   "Tariff measures" = "TARIFF",
                   "Capital control measures" = "CAP",
                   "FDI measures" = "FDI",
                   "Migration measures" = "MIG",
                   "Instrument unclear" = "X"
)

source(paste0(path,"/code/modules/updateCoverages.R"))
source(paste0(path,"/code/modules/updateDataCounts.R"), local = T)

load("data/master_plus.Rdata")
master.cols <- colnames(master)
rm(master)


ui <- function(request) { fluidPage(
  theme = "style.css",
  tags$head(tags$link(rel="stylesheet", type="text/css", href="tipped.css")),
  tags$head(tags$script(src="tipped.js")),
  
  # HEADER
  HTML("<div class='overall-wrap'><div class='header'><div class='header-inner'><div class='title'><h1>GTA data kitchen</h1><h2>beta</h2></div><div class='gta-logo'><a href='http://globaltradealert.org' target='_blank'>"),
  img(src='GTA-LOGO-light-grey.svg'),
  HTML("</a></div></div></div>"),
  
  # SETTINGS WRAP
  HTML("<div id='settings' class='settings'>
       <div class='app-switcher'>
       <div class='tab-nav-wrapper'>
       <ul class='tab-list nav'>
       <li id='toggle-data-count' class='active'><a data-toggle='tab' href='#data-counts'>Count statistics</a></li>
       <li id='toggle-coverages' class=''><a data-toggle='tab' href='#trade-coverages'>Trade coverage shares</a></li>
       </ul>
       </div>
       </div>
       <div class='settings-wrap'>
       <div class='settings-general'>"),
  
  tags$div(id="app-status-infos",
           tags$div(class = "settings-title title-top", checked = NA,
                    tags$h2("App Status")
           ),
           
           fluidRow(
             column(4,
                    tags$label(class = "control-label", checked = NA,
                               "Queue Information"
                    ),
                    textOutput("queue")),
             column(4,
                    tags$label(class = "control-label", checked = NA,
                               "Remove orders"
                    ),
                    uiOutput("list_of_orders"),
                    actionButton("remove.order",
                                 "Remove selected orders")),
             column(6,
                    tags$label(class = "control-label", checked = NA,
                               "Update Underlying GTA Data"
                    ),
                    textOutput("age"),
                    actionButton("update.gta",
                                 "Update underlying GTA data"))
           )),
  
  ######## DATA COUNTS #########
  
  HTML("<div class='tab-pane active fade in' id='data-counts'>"),
  
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Use a query template")
  ),
  fluidRow(
    column(12,
           HTML("<p><strong>Optional: Choose from the list of common GTA data inquieries below.</strong><br />
                The Data Count Setting below will update as if to perform the selected query. Note that you can tailor the selected query to your need by changing the relevant setting field accordingly."),
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Pre-defined queries</span>
                    Work with a pre-defined state of the app.
                    ",
                    tags$p("?")),
           selectInput("predefined.data.count",
                       label=NULL,
                       choices = c("None" = "Unset",
                                   "Query 1" = "query_1"),
                       selected = "Unset"))
    ),
  
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Data Counts Settings")
  ),
  
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>GTA evaluation</span>
                    Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
                    ",
                    tags$p("?")),
           selectInput("gta.evaluation.data.count",
                       "Restrict GTA evaluation",
                       c("Red", "Amber", "Green"),
                       multiple = T)
    ),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Affected Flow</span>
                    Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
                    ",
                    tags$p("?")),
           selectInput("affected.flows.data.count",
                       "Restrict affected flow",
                       c("Inward", "Outward", "Outward subsidy"),
                       selected = c("Inward","Outward subsidy"),
                       multiple =T)
    )
    ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict implementers</span>
                    Specify the implementing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
                    <span>Keep implementers</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
                    ",
                    tags$p("?")),
           multiInput("implementers.data.count",
                      "Restrict implementers",
                      choices = c("All countries" = "all", c(as.character(unique(countries$name)))),
                      selected = "all"),
           checkboxInput("keep.implementer.data.count",
                         "Keep selected implementers?",
                         value = T),
           checkboxInput("group.implementer.data.count",
                         "Group selected implementers?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict affected countries</span>
                    Specify the affected countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
                    <span>Keep affected countries</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated affected country.
                    <span>Keep other countries</span>
                    Specify whether to keep the data for the other jurisdictions that happen to be affected alongside those you specified (T/F). Default is 'TRUE'.
                    <span>Nr of affected countries included</span>
                    Specify whether to include interventions that affect only one of the selected affected jurisdictions ('One'), at least one of the selected affected jurisdictions ('One plus') or all of the selected affected jurisdictions ('All'). Default is 'One plus'.                    
                    ",
                    tags$p("?")),
           multiInput("affected.data.count",
                      "Restrict affected countries",
                      choices = c("All countries" = "all", c(as.character(unique(countries$name)))),
                      selected = "all"),
           checkboxInput("keep.affected.data.count",
                         "Keep selected affected jurisdictions?",
                         value = T),
           checkboxInput("keep.others.data.count",
                         "Keep other affected jurisdictions?",
                         value = F),
           checkboxInput("group.affected.data.count",
                         "Group selected affected jurisdictions?",
                         value = T),
           selectInput("incl.affected.strictness.data.count",
                       "Include interventions including ... of the selected countries",
                       choices = c("One or more" = "ONEPLUS", "Only one" = "ONE", "All" = "ALL"),
                       selected = "ONEPLUS"))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>In force today</span>
                    Specify whether you want to focus on interventions in force today ('TRUE') or no longer in force today ('FALSE'). Default is 'any'.
                    ",
                    tags$p("?")),
           selectInput("in.force.today.data.count",
                       "Interventions in force today?",
                       c("Yes", "No", "Any"),
                       selected = "Any")),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Nr of affected countries</span>
                    Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(0,999).                    
                    <span>Calculate nr of affected countries</span>
                    Specify whether the number of importers affected by an intervention is calculated based only on the selected importers included ('Selected only'), only on the unselected importers ('Unselected only') or based on both ('All'). Default is 'All'.
                    ",
                    tags$p("?")),
           textInput("nr.affected.data.count",
                     "Supply range for number of countries affected",
                     placeholder = "0,999"),
           selectInput("nr.affected.incl.data.count",
                       "What affected countries should be included in this count?",
                       choices = c("All" = "ALL","Selected only" = "SELECTED","Unselected only" = "UNSELECTED"),
                       selected = "ALL"))
  ),
  fluidRow(
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Announcement Period</span>
                    Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("announcement.period.data.count",
                          "Announcement period",
                          start = "0000-00-00",
                          end = "0000-00-00")),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Implementation Period</span>
                    Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
                    <span>Keep Implementation NA</span>
                    Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing implementation.date
                    ",
                    tags$p("?")),
           dateRangeInput("implementation.period.data.count",
                          "Implementation period",
                          start = "0000-00-00",
                          end = "0000-00-00"),
           checkboxInput("keep.implementation.na.data.count",
                         "Keep interventions without implementation date?",
                         value = T)),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Revocation Period</span>
                    Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
                    <span>Keep Revocation NA</span>
                    Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing revocation.date.                    ",
                    tags$p("?")),
           dateRangeInput("revocation.period.data.count",
                          "Revocation period",
                          start = "0000-00-00",
                          end = "0000-00-00"),
           checkboxInput("keep.revocation.na.data.count",
                         "Keep interventions without revocation date?",
                         value = T)),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Submission Period</span>
                    Specify a period in which the interventions for your analysis have been submitted Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions submitted since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("submission.period.data.count",
                          "Submission period",
                          start = "0000-00-00",
                          end = "0000-00-00"))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restricted intervention types</span>
                    Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
                    <span>Keep intervention type</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
                    <span>Group intervention type</span>
                    Specify whether to aggregate the statistics for all remaining intervention types into one group (TRUE) or whether create the statistics for every single type (FALSE). Default is TRUE.
                    ",
                    tags$p("?")),
           multiInput("intervention.types.data.count",
                      "Restrict intervention types",
                      c("All types" = "all", as.character(unique(intervention_types$intervention.type))),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.type.data.count",
                         "Keep the selected intervention types?",
                         value = T),
           checkboxInput("group.type.data.count",
                         "Group selected intervention types?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restricted MAST chapters</span>
                    Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
                    <span>Keep MAST chapters</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
                    ",
                    tags$p("?")),
           multiInput("mast.chapters.data.count",
                      "Restrict MAST chapters",
                      c("All chapters" = "all", mast_chapters),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.mast.data.count",
                         "Keep the selected MAST chapters?",
                         value = T),
           checkboxInput("group.mast.data.count",
                         "Group selected MAST chapters?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict implementation levels</span>
                    Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
                    <span>Keep implementation levels</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
                    ",
                    tags$p("?")),
           multiInput("implementation.level.data.count",
                      "Restrict the implementation level",
                      c('any', 'supranational', 'national', 'subnational', 'IFI', 'NFI'),
                      selected = 'any'),
           checkboxInput("keep.level.data.count",
                         "Keep the selected implementation levels?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict eligible firms</span>
                    Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
                    <span>Keep eligible firms</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
                    ",
                    tags$p("?")),
           multiInput("eligible.firms.data.count",
                      "Restrict eligible firms",
                      c('any', 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise'),
                      selected = "any"),
           checkboxInput("keep.firms.data.count",
                         "Keep the selected eligible firms?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict CPC sectors</span>
                    Provide a vector of CPC codes that you are interested in (version 2.1, any digit level).
                    <span>Keep CPC codes</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
                    ",
                    tags$p("?")),
           multiInput("cpc.sectors.data.count",
                      "Restrict CPC sectors",
                      c("All sectors" = "all", "All service sectors" = "all_service", "All goods sectors" = "all_goods", product.groups, sectors_list),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.cpc.data.count",
                         "Keep the selected CPC sectors?",
                         value = T),
           checkboxInput("group.cpc.data.count",
                         "Group selected CPC sectors?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict HS products</span>
                    Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
                    <span>Keep HS codes</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
                    ",
                    tags$p("?")),
           multiInput("hs.codes.data.count",
                      "Restrict HS codes",
                      c("All HS codes" = "all", products_list),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.hs.data.count",
                         "Keep the selected HS codes?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Intervention IDs</span>
                    Provide a vector of intervention IDs.
                    ",
                    tags$p("?")),
           textInput("intervention.ids.data.count",
                     "Restrict to the following intervention IDs:"),
           checkboxInput("keep.interventions.data.count",
                         "Keep the selected interventions?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Lag adjustment</span>
                    Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
                    ",
                    tags$p("?")),
           dateInput("lag.adjustment.data.count",
                     "Adjust for the reporting lag",
                     min = as.Date(paste0(year(Sys.Date()), "-01-01")),
                     max = as.Date(paste0(year(Sys.Date()), "-12-31")),
                     format = "mm-dd",
                     value = ""))
  ),
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Choose aggregation settings")
  ),
  
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Variable to be counted</span>
                    Select one of the 4 variables you want to count.
                    <span>Count variable by</span>
                    Specify by which parameters you want to group the chosen variable. One or more one parameters can be chosen.
                    <span>Type of count</span>
                    Value cannot be changed.
                    ",
                    tags$p("?")),
           selectInput("aggregate.y.data.count",
                       "Select variable to be counted",
                       column.list.numeric,
                       multiple = F),
           selectInput("aggregate.x.data.count",
                       "Select X variables for aggregation",
                       column.list,
                       multiple = T,
                       selected = c("Implemented (year)" = "year(date.implemented)")),
           selectInput("aggregate.style.data.count",
                       "Select the statistic you want to produce",
                       c("Number of unique values" = "length(unique(x))"),
                       multiple = F)
    ),
    column(6,
           tags$label(class = "control-label", checked = NA,
                      "Choose Output Types"
           ),
           checkboxInput("xlsx.data.count",
                         "Excel file",
                         value = T),
           checkboxInput("interventions.list.data.count",
                         "Interventions detailed list",
                         value = F),
           checkboxInput("line.data.count",
                         "Line Chart",
                         value = F),
           checkboxInput("bar.data.count",
                         "Bar Chart",
                         value = F),
           checkboxInput("map.data.count",
                         "Map",
                         value = F),
           checkboxInput("tile.data.count",
                         "Tile Chart",
                         value = F))
    ),
  conditionalPanel(condition = "input['line.data.count'] == true",
                   tags$div(class = "settings-title", checked = NA,
                            tags$h2("Line chart settings")
                   ),
                   fluidRow(
                     column(6,
                            textInput("line.title.data.count",
                                      "Type in plot title"),
                            textInput("line.legend.title.data.count",
                                      "Type in legend title"),
                            textInput("line.x.axis.title.data.count",
                                      "Type in X axis title"),
                            textInput("line.y.axis.title.data.count",
                                      "Type in Y axis title"),
                            selectInput("line.colour.data.count",
                                        "Choose colour palette",
                                        c("Qualitative" = "gta_colour$qualitative","Harmful" = "gta_colour$red.shades","Liberating" = "gta_colour$green.shades"),
                                        multiple = F)),
                     column(6,
                            selectInput("line.x.var.data.count",
                                        "Select X axis data column",
                                        column.list,
                                        multiple = F),
                            selectInput("line.y.var.data.count",
                                        "Select Y axis data column",
                                        column.list.numeric,
                                        multiple = F),
                            selectInput("line.group.var.data.count",
                                        "Select group variable data column",
                                        c("", column.list),
                                        selected = NULL,
                                        multiple = F))
                   )),
  conditionalPanel(condition = "input['bar.data.count'] == true",
                   tags$div(class = "settings-title", checked = NA,
                            tags$h2("Bar chart settings")
                   ),
                   fluidRow(
                     column(6,
                            textInput("bar.title.data.count",
                                      "Type in plot title"),
                            textInput("bar.legend.title.data.count",
                                      "Type in legend title"),
                            textInput("bar.x.axis.title.data.count",
                                      "Type in X axis title"),
                            textInput("bar.y.axis.title.data.count",
                                      "Type in Y axis title"),
                            selectInput("bar.colour.data.count",
                                        "Choose colour palette",
                                        c("Qualitative" = "gta_colour$qualitative","Harmful" = "gta_colour$red.shades","Liberating" = "gta_colour$green.shades"),
                                        multiple = F)),
                     column(6,
                            selectInput("bar.x.var.data.count",
                                        "Select X axis data column",
                                        column.list,
                                        multiple = F),
                            selectInput("bar.y.var.data.count",
                                        "Select Y axis data column",
                                        column.list.numeric,
                                        multiple = F),
                            selectInput("bar.group.var.data.count",
                                        "Select group variable data column",
                                        c("", column.list),
                                        selected = NULL,
                                        multiple = F)))
  ),
  conditionalPanel(condition = "input['map.data.count'] == true",
                   tags$div(class = "settings-title", checked = NA,
                            tags$h2("Map settings")
                   ),
                   fluidRow(
                     column(6,
                            textInput("map.title.data.count",
                                      "Type in plot title"),
                            textInput("map.legend.title.data.count",
                                      "Type in legend title"),
                            selectInput("map.colour.low.data.count",
                                        "Choose the color for low values",
                                        colour.list,
                                        multiple = F),
                            selectInput("map.colour.high.data.count",
                                        "Choose the colour for high values",
                                        colour.list,
                                        multiple = F)),
                     column(6,
                            selectInput("map.countries.data.count",
                                        "Select Country data column",
                                        column.list,
                                        multiple = F),
                            selectInput("map.value.data.count",
                                        "Select value data column",
                                        column.list.numeric,
                                        multiple = F),
                            numericInput("map.splits.data.count",
                                        "Choose number of coloring shades",
                                        value = 3),
                            textInput("map.brackets.data.count",
                                      "Define bracket boundaries (will override number of coloring shades)",
                                       placeholder = "e.g. '1,20,40'")
                            ))
  ),
  conditionalPanel(condition = "input['tile.data.count'] == true",
                   tags$div(class = "settings-title", checked = NA,
                            tags$h2("Tile chart settings")
                   ),
                   fluidRow(
                     column(6,
                            textInput("tile.title.data.count",
                                      "Type in plot title"),
                            textInput("tile.legend.title.data.count",
                                      "Type in legend title"),
                            textInput("tile.x.axis.title.data.count",
                                      "Type in X axis title"),
                            textInput("tile.y.axis.title.data.count",
                                      "Type in Y axis title"),
                            selectInput("tile.colour.low.data.count",
                                        "Choose the color for low values",
                                        colour.list,
                                        multiple = F),
                            selectInput("tile.colour.high.data.count",
                                        "Choose the colour for high values",
                                        colour.list,
                                        multiple = F)),
                     column(6,
                            selectInput("tile.x.var.data.count",
                                        "Select X axis data column",
                                        column.list,
                                        multiple = F),
                            selectInput("tile.y.var.data.count",
                                        "Select Y axis data column",
                                        column.list,
                                        multiple = F),
                            selectInput("tile.value.var.data.count",
                                        "Select value variable data column",
                                        column.list.numeric,
                                        multiple = F)))
  ),
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Submit order")
  ),
  
  fluidRow(
    column(4,
           textInput("order.email.data.count",
                     "Please provide email address"),
           textInput("order.name.data.count",
                     "Optional: Choose a name for your order")),
    column(4,
           HTML("<div class='top_of_queue'>"),
           checkboxInput("top.of.queue.data.count",
                         "Push to the front of the queue?",
                         value = F),
           HTML("</div>")),
    column(4,
           tags$div(class="generate_file-wrap", checked = NA,
                    tags$div(class="generate_file-inner", checked = NA,
                             actionButton("generate_file_data_count",
                                          "Submit your order"))))
  ),
  
  HTML("</div>"),
  
  ######## TRADE COVERAGES #########
  
  HTML("<div class='tab-pane fade' id='trade-coverages'>"),
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Use a query template")
  ),
  fluidRow(
    column(12,
           HTML("<p><strong>Optional: Choose from the list of common GTA data inquieries below.</strong><br />
                The Coverage Settings below will update as if to perform the selected query. Note that you can tailor the selected query to your need by changing the relevant setting field accordingly."),
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Pre-defined queries</span>
                    Work with a pre-defined state of the app.
                    ",
                    tags$p("?")),
           selectInput("predefined.coverages",
                       label=NULL,
                       choices = c("None" = "Unset",
                                   "Query 1" = "query_1")))
    ),
  
  
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Coverage Settings")
  ),
  
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Coverage Period</span>
                    The calendar years for which to calculate the trade coverage shares. Default is c(2009,CURRENT.YEAR). Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Use implementation.period parameter to change this.
                    <span>Current year to date</span>
                    Should the coverage statistics for the current year be calculated as 'coverage for year to date' (TRUE) or 'coverage for entire current year' (FALSE). Default is TRUE.
                    ",
                    tags$p("?")),
           sliderInput("coverage.period",
                       "Coverage period",
                       min = 2009, max = year(Sys.Date()),
                       value = c(2009, year(Sys.Date())),
                       step=1, sep = ""),
           checkboxInput("current.year.todate",
                         paste("For ",year(Sys.Date()),", use share of year to date that interventions were in force?"),
                         value = T)
    ),
    column(6,
           selectInput("choose.tradebase",
                       "Trade weights based on data from",
                       choices = c("GTA base years" = "base",
                                   "2007" = "2007",
                                   "2008" = "2008",
                                   "2009" = "2009",
                                   "2010" = "2010",
                                   "2011" = "2011",
                                   "2012" = "2012",
                                   "2013" = "2013",
                                   "2014" = "2014",
                                   "2015" = "2015",
                                   "2016" = "2016",
                                   "2017" = "2017"),
                       multiple = F,
                       selected = "base"),
           selectInput("choose.output",
                       "Choose trade statistic",
                       choices=c("Shares" = "share",
                                 "USD value" = "value"),
                       multiple = F,
                       selected = "share"),
           checkboxInput("adjust.for.intra",
                         "Adjust for intra-year duration",
                         value = T)
    )),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>GTA Evaluation</span>
                    Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
                    ",
                    tags$p("?")),
           selectInput("gta.evaluation",
                       "Restrict GTA evaluation",
                       c("Red", "Amber", "Green"),
                       multiple = T)
    ),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Affected Flow</span>
                    Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
                    ",
                    tags$p("?")),
           selectInput("affected.flows",
                       "Restrict affected flow",
                       c("Inward", "Outward", "Outward subsidy"),
                       selected = c("Inward","Outward subsidy"),
                       multiple =T)
    )
    ),
  fluidRow(
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Exporters</span>
                    Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. Default: All exporters.
                    <span>Group Exporters</span>
                    Specify whether to aggregate the statistics for all remaining exporters into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
                    <span>Keep Exporters</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated exporters.
                    <span>Separate Country Groups</span>
                    Specify whether to calculate values for country groups separately ('TRUE') or not ('FALSE').
                    <span>Nr of exporters included</span>
                    Specify whether to include interventions that affect only one of the selected exporters ('One'), at least one of the selected exporters ('One plus') or all of the selected exporters ('All'). Default is 'One plus'
                    ",
                    tags$p("?")),
           multiInput("exporters",
                      "Restrict exporters",
                      choices = c("All countries" = "all", c(as.character(unique(countries$name)))),
                      selected = "all"),
           checkboxInput("group.exporters",
                         "Group selected exporters together?",
                         value = T),
           checkboxInput("keep.exporters",
                         "Keep selected exporters?",
                         value = T),
           checkboxInput("separate.exporter.groups",
                         "Separately calculate country groups?",
                         value = F),
           selectInput("incl.exporters.strictness",
                       "Include interventions including ... of the selected countries",
                       choices = c("One or more" = "ONEPLUS", "Only one" = "ONE", "All" = "ALL"),
                       selected = "ONEPLUS")),
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Importers</span>
                    Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. Default: All importers.
                    <span>Group Importers</span>
                    Specify whether to aggregate the statistics for all remaining importers into one group (TRUE) or whether create the statistics for every single one (FALSE). Default is TRUE.
                    <span>Keep Importers</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated importers
                    <span>Separate Country Groups</span>
                    Specify whether to calculate values for country groups separately ('TRUE') or not ('FALSE').
                    <span>Nr of importers included</span>
                    Specify whether to include interventions that affect only one of the selected importers ('One'), at least one of the selected importers ('One plus') or all of the selected importers ('All'). Default is 'One plus'
                    ",
                    tags$p("?")),
           multiInput("importers",
                      "Restrict importers",
                      choices = c("All countries" = "all", c(as.character(unique(countries$name)))),
                      selected = "all"),
           checkboxInput("group.importers",
                         "Group selected importers together?",
                         value = T),
           checkboxInput("keep.importers",
                         "Keep selected importers?",
                         value = T),
           checkboxInput("separate.importer.groups",
                         "Separately calculate country groups?",
                         value = F),
           selectInput("incl.importers.strictness",
                       "Include interventions including ... of the selected countries",
                       choices = c("One or more" = "ONEPLUS", "Only one" = "ONE", "All" = "ALL"),
                       selected = "ONEPLUS")),
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Implementers</span>
                    Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
                    <span>Implementer role</span>
                    Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
                    ",
                    tags$p("?")),
           multiInput("implementers",
                      "Restrict implementers",
                      choices = c("All countries" = "all", c(as.character(unique(countries$name)))),
                      selected = "all"),
           checkboxInput("keep.implementer",
                         "Keep selected implementers?",
                         value = T),
           checkboxGroupInput("implementer.role",
                              "Choose implementer's role",
                              c("Importer", "Exporter", "3rd country"),
                              selected = c("Importer", "3rd country"))
    )
    ),
  fluidRow(
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Number of exporters</span>
                    Specify the range for the number of exporters affected by an intervention. Default is any number i.e. c(1,999)                    
                    <span>Number of exporters included</span>
                    Specify whether in the number of exporters affected by an intervention is calculated based only on the selected exporters are included ('Selected only'), only on the unselected exporters ('Unselected only') or based on both ('All'). Default is 'All'.
                    ",
                    tags$p("?")),
           textInput("nr.exporters",
                     "Supply range for how many exporters may be affected",
                     placeholder = "0,999"),
           selectInput("nr.exporters.incl",
                       "What affected exporters should be included in this count?",
                       choices = c("All" = "ALL","Selected only" = "SELECTED","Unselected only" = "UNSELECTED"),
                       selected = "ALL")),
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Number of importers</span>
                    Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(1,999)                    
                    <span>Number of importers included</span>
                    Specify whether in the number of importers affected by an intervention is calculated based only on the selected importers are included ('Selected only'), only on the unselected importers ('Unselected only') or based on both ('All'). Default is 'All'.
                    ",
                    tags$p("?")),
           textInput("nr.importers",
                     "Supply range for how many importers may be affected",
                     placeholder = "0,999"),
           selectInput("nr.importers.incl",
                       "What affected importers should be included in this count?",
                       choices = c("All" = "ALL","Selected only" = "SELECTED","Unselected only" = "UNSELECTED"),
                       selected = "ALL")),
    column(4,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Number of interventions</span>
                    Specify whether to calculate the trade shares by the number of interventions affecting a importer-exporter-product combination e.g. '1,2,3,4,5,999999' for the brackets '1-2,3-4,5 or more'. Default is '1,99999'.
                    ",
                    tags$p("?")),
           textInput("hit.brackets",
                     "Breakdown by number of interventions",
                     placeholder = "1,99999"))
  ),
  fluidRow(
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Announcement Period</span>
                    Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("announcement.period",
                          "Announcement period",
                          start = "0000-00-00",
                          end = "0000-00-00")),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Implementation Period</span>
                    Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("implementation.period",
                          "Implementation period",
                          start = "0000-00-00",
                          end = "0000-00-00")),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Revocation Period</span>
                    Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("revocation.period",
                          "Revocation period",
                          start = "0000-00-00",
                          end = "0000-00-00"),
           checkboxInput("keep.revocation.na",
                         "Keep interventions without revocation date?",
                         value = T)),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Submission Period</span>
                    Specify a period in which the interventions for your analysis have been submitted Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions submitted since 'after.date'.
                    ",
                    tags$p("?")),
           dateRangeInput("submission.period",
                          "Submission period",
                          start = "0000-00-00",
                          end = "0000-00-00")),
    column(3,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>In force today</span>
                    Specify whether you want to focus on interventions in force today ('TRUE') or no longer in force today ('FALSE'). Default is 'any'.
                    ",
                    tags$p("?")),
           selectInput("in.force.today",
                       "Should interventions be in force today?",
                       c("Yes", "No", "Any"),
                       selected = "Any"))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restricted intervention types</span>
                    Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
                    <span>Keep intervention type</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
                    <span>Group intervention type</span>
                    Specify whether to aggregate the statistics for all remaining intervention types into one group (TRUE) or whether create the statistics for every single type (FALSE). Default is TRUE.
                    ",
                    tags$p("?")),
           multiInput("intervention.types",
                      "Restrict the intervention types",
                      c("All types" = "all", as.character(unique(intervention_types$intervention.type))),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.type",
                         "Keep the selected intervention types?",
                         value = T),
           checkboxInput("group.type",
                         "Group the selected intervention types?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restricted MAST chapters</span>
                    Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
                    <span>Keep MAST chapters</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
                    <span>Group Mast Chapters</span>
                    Specify whether to aggregate the statistics for all remaining MAST chapters into one group (TRUE) or whether create the statistics for every single chapter (FALSE). Default is TRUE.
                    ",
                    tags$p("?")),
           multiInput("mast.chapters",
                      "Restrict the MAST chapters",
                      c("All chapters" = "all", mast_chapters),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.mast",
                         "Keep the selected MAST chapters?",
                         value = T),
           checkboxInput("group.mast",
                         "Group the selected MAST chapters?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict implementation levels</span>
                    Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
                    <span>Keep implementation levels</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
                    ",
                    tags$p("?")),
           multiInput("implementation.level",
                      "Restrict the implementation level",
                      c('any', 'supranational', 'national', 'subnational', 'IFI', 'NFI'),
                      selected = 'any'),
           checkboxInput("keep.level",
                         "Keep the selected implementation levels?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict eligible firms</span>
                    Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
                    <span>Keep eligible firms</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
                    ",
                    tags$p("?")),
           multiInput("eligible.firms",
                      "Restrict the eligible firms",
                      c('any', 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise'),
                      selected = "any"),
           checkboxInput("keep.firms",
                         "Keep the selected eligible firms?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict CPC sectors</span>
                    Provide a vector of CPC codes that you are interested in (version 2.1, any digit level).
                    <span>Keep CPC codes</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
                    ",
                    tags$p("?")),
           multiInput("cpc.sectors",
                      "Restrict the CPC sectors",
                      c("All sectors" = "all", "All service sectors" = "all_service", "All goods sectors" = "all_goods", product.groups, sectors_list),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.cpc",
                         "Keep the selected CPC sectors?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Restrict HS products</span>
                    Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
                    <span>Keep HS codes</span>
                    Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
                    ",
                    tags$p("?")),
           multiInput("hs.codes",
                      "Restrict the HS codes",
                      c("All HS codes" = "all", products_list),
                      selected = "all",
                      width = "100%"),
           checkboxInput("keep.hs",
                         "Keep the selected HS codes?",
                         value = T))
  ),
  fluidRow(
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Intervention IDs</span>
                    Provide a vector of intervention IDs.
                    ",
                    tags$p("?")),
           textInput("intervention.ids",
                     "Restrict to the following intervention IDs:"),
           checkboxInput("keep.interventions",
                         "Keep the selected intervention IDs?",
                         value = T)),
    column(6,
           tags$div(class = "create-tooltip help",
                    title = "
                    <span>Lag adjustment</span>
                    Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
                    ",
                    tags$p("?")),
           dateInput("lag.adjustment",
                     "Adjust for the reporting lag",
                     min = as.Date(paste0(year(Sys.Date()), "-01-01")),
                     max = as.Date(paste0(year(Sys.Date()), "-12-31")),
                     format = "mm-dd",
                     value = ""))
  ),
  
  tags$div(class = "settings-title", checked = NA,
           tags$h2("Submit order")
  ),
  
  fluidRow(
    column(4,
           textInput("order.email",
                     "Please provide email address"),
           textInput("order.name",
                     "Choose a name for your order")),
    column(4,
           HTML("<div class='top_of_queue'>"),
           checkboxInput("top.of.queue",
                         "Push to the front of the queue?",
                         value = F),
           checkboxInput("xlsx.interventions",
                         "Generate a list with included interventions?",
                         value = T),
           HTML("</div>")),
    column(4,
           tags$div(class="generate_file-wrap", checked = NA,
                    tags$div(class="generate_file-inner", checked = NA,
                             actionButton("generate_file",
                                          "Submit your order"))))
  ),
  
  # tableOutput("show_inputs")
  
  HTML("</div>"),
  
  
  
  # CLOSE SETTINGS
  
  tags$script(src="tooltips.js"),
  tags$script(src="app.js"),
  HTML("</div></div></div></div>")
    ) }

######## SERVER #########

# Function enabling rbinding if there are different number of columns present
server <- function(input, output, session) {
  
  observeEvent(input$predefined.data.count, {
    
    callUpdateDataCounts(session = session,
                         query = input$predefined.data.count)
    
  })
  
  
  observeEvent(input$predefined.coverages, {
    
    callUpdateCoverages(session = session,
                        query = input$predefined.coverages)
    
  })
  
  
  
  rbind.all.columns <- function(x, y) {
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    y[, c(as.character(x.diff))] <- NA
    
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    
    return(rbind(x, y))
  }
  
  ###### TRADE COVERAGE ######
  
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    test = data.frame(
      ticket.number=NA,
      order.type="trade coverage",
      time.order=Sys.time(),
      time.start=NA,
      time.finish=NA,
      under.preparation=1,
      values = lapply(x, function(y) paste(unlist(y), collapse = ","))
    )
    test <- test %>% select(-ends_with(".data.count"))
    names(test)=gsub("values.", "", names(test))
    test$generate_file <- NULL
    test$generate_file_data_count <- NULL
    test$update.gta <- NULL
    test
  })
  
  # output$show_inputs <- renderTable({
  #   AllInputs()
  # })
  
  output$queue <- renderText({
    load(paste0(path,"log/kitchen log.Rdata"))
    paste("Length of current queue:", sum(subset(kitchen.log, is.na(under.preparation)==F)$under.preparation))
  })
  
  output$list_of_orders <- renderUI({
    load(paste0(path,"log/kitchen log.Rdata"))
    list.queue <- subset(kitchen.log, under.preparation == 1)$ticket.number
    selectInput("order_remove",
                label=NULL,
                choices = list.queue,
                multiple=T)
  })
  
  output$age <- renderText({
    paste("Underlying GTA data as of ", file.info("data/master_plus.Rdata")$mtime, "GMT")
  })
  
  observeEvent(input$generate_file, {
    if(grepl("@", input$order.email)){
      showNotification("The requested trade coverage calculation was received.", type = "message", duration = NA)
      load(paste0(path,"log/kitchen log.Rdata"))
      test <- AllInputs()
      test <- as.data.frame(test)
      kitchen.log <- rbind.all.columns(kitchen.log, test)
      kitchen.log$ticket.number[nrow(kitchen.log)] <- nrow(kitchen.log)
      kitchen.log$order.type <- as.character(kitchen.log$order.type)
      save(kitchen.log, file = paste0(path,"log/kitchen log.Rdata"))
      
      
      test <- as.data.frame(t(subset(kitchen.log, ticket.number==nrow(kitchen.log))))
      test$text <- paste(row.names(test), test[,1], sep = ": ")
      
      sender = "data@globaltradealert.org"
      recipients = as.character(kitchen.log$order.email[nrow(kitchen.log)])
      sbjct=paste("Thank you for your order [GTA data dish #",kitchen.log$ticket.number[nrow(kitchen.log)],"]",sep="")
      message=paste("Hello<p>Thank you for your order. The requested file is being calculated and you will receive a ",
                    "separate email as soon as it is finished.<br>In case of questions or suggestions, please reply to ",
                    "this message.<p>Regards<br>Global Trade Alert Data<br><br>Query overview:<br>", paste(test$text, collapse = "<br>"), sep = "")
      
      
      
      send.mail(from = sender,
                to = recipients,
                subject=sbjct,
                body=message,
                html=T,
                # attach.files = falls ihr die parameter als XLSX anhaengt; im mail waere besser,
                smtp = list(host.name = "mail.infomaniak.com",
                            port=587,
                            user.name=sender,
                            passwd="B0d@nstrasse",
                            tls=T),
                authenticate = T)
      
      output$queue <- renderText({
        load(paste0(path,"log/kitchen log.Rdata"))
        paste("Length of current queue:", sum(subset(kitchen.log, is.na(under.preparation)==F)$under.preparation))
        
        showNotification("The requested trade coverage calculation was received.", type = "message", duration = NA)
      })
      
    }
  })
  
  observeEvent(input$update.gta,{
    load(paste0(path,"log/kitchen log.Rdata"))
    kitchen.log[nrow(kitchen.log)+1,] <- NA
    kitchen.log$ticket.number[nrow(kitchen.log)] <- nrow(kitchen.log)
    kitchen.log$time.order[nrow(kitchen.log)] <- Sys.time()
    kitchen.log$order.type <- as.character(kitchen.log$order.type)
    kitchen.log$order.type[nrow(kitchen.log)] <- "GTA"
    kitchen.log$under.preparation[nrow(kitchen.log)] <- 1
    
    save(kitchen.log, file = paste0(path,"log/kitchen log.Rdata"))
    
    output$age <- renderText({
      paste("Current file was generated on:", file.info("data/master_plus.Rdata")$mtime)
    })
    
    showNotification("GTA data will be updated.", type = "message", duration = NA)
    
  })
  
  observeEvent(input$remove.order,{
    load(paste0(path,"log/kitchen log.Rdata"))
    kitchen.log$under.preparation[kitchen.log$ticket.number %in% as.numeric(unlist(strsplit(input$order_remove,",")))] <- 0
    
    save(kitchen.log, file = paste0(path,"log/kitchen log.Rdata"))
    
    showNotification(paste("The following orders will be canceled: ", as.numeric(unlist(strsplit(input$order_remove,","))), collapse = ","), type = "message", duration = NA)
    
  })
  
  
  ###### DATA COUNTS #######
  
  AllInputs.data.count <- reactive({
    x <- reactiveValuesToList(input)
    test = data.frame(
      ticket.number=NA,
      order.type="count statistics",
      time.order=Sys.time(),
      time.start=NA,
      time.finish=NA,
      under.preparation=1,
      values = lapply(x, function(y) paste(unlist(y), collapse = ","))
    )
    test.temp <- test %>% select(ends_with(".data.count"))
    test <- test %>% select(-starts_with("values."))
    test <- cbind(test, test.temp)
    names(test)=gsub("values.", "", names(test))
    names(test)=gsub(".data.count", "", names(test))
    test$generate_file <- NULL
    test$generate_file_data_count <- NULL
    test$update.gta <- NULL
    test$update.gta.data.count <- NULL
    test
  })
  
  observeEvent(input$generate_file_data_count, {
    if(grepl("@", input$order.email.data.count)){
      load(paste0(path,"log/kitchen log.Rdata"))
      test <- AllInputs.data.count()
      test <- as.data.frame(test)
      kitchen.log <- rbind.all.columns(kitchen.log, test)
      kitchen.log$ticket.number[nrow(kitchen.log)] <- nrow(kitchen.log)
      kitchen.log$order.type <- as.character(kitchen.log$order.type)
      save(kitchen.log, file = paste0(path,"log/kitchen log.Rdata"))
      
      
      test <- as.data.frame(t(subset(kitchen.log, ticket.number==nrow(kitchen.log))))
      test$text <- paste(row.names(test), test[,1], sep = ": ")
      
      sender = "data@globaltradealert.org"
      recipients = as.character(kitchen.log$order.email[nrow(kitchen.log)])
      sbjct=paste("Thank you for your order [GTA data dish #",kitchen.log$ticket.number[nrow(kitchen.log)],"]",sep="")
      message=paste("Hello<p>Thank you for your order. The requested file is being calculated and you will receive a ",
                    "separate email as soon as it is finished.<br>In case of questions or suggestions, please reply to ",
                    "this message.<p>Regards<br>Global Trade Alert Data<br><br>Query overview:<br>", paste(test$text, collapse = "<br>"), sep = "")
      
      send.mail(from = sender,
                to = recipients,
                subject=sbjct,
                body=message,
                html=T,
                # attach.files = falls ihr die parameter als XLSX anhaengt; im mail waere besser,
                smtp = list(host.name = "mail.infomaniak.com",
                            port=587,
                            user.name=sender,
                            passwd="B0d@nstrasse",
                            tls=T),
                authenticate = T)
      
      output$queue <- renderText({
        load(paste0(path,"log/kitchen log.Rdata"))
        paste("Length of current queue:", sum(subset(kitchen.log, is.na(under.preparation)==F)$under.preparation))
        
        showNotification("The requested data count calculation was received.", type = "message", duration = NA)
      })
      
    }
  })
  
  
  
}


shinyApp(ui = ui, server = server)

# load(file = "17 Shiny/4 data kitchen/log/kitchen log TEST.Rdata")
# test <- kitchen.log
# load(file = "17 Shiny/4 data kitchen/log/kitchen log.Rdata")
