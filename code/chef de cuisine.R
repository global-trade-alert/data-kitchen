library("httr")
library("splitstackshape")
library("foreign")
library("openxlsx")
library("gtalibrary")
library("data.table")
library("ggplot2")
library(mailR)
rm(list = ls())
# copying log over to the cloud
file.copy("/home/rstudio/ShinyApps/data-dev/cdf.log",
          "/home/rstudio/Dropbox/GTA cloud/0 dev/data-kitchen-pb/log/chef de cuisine.log",overwrite = T)

path = "17 Shiny/4 data kitchen/"
# path = "0 dev/data-kitchen-pb/"

## check if a process is running on the server
running.processes=system("ps aux", intern=T)

load(paste0("/home/rstudio/Dropbox/GTA cloud/",path,"log/kitchen log.Rdata"))
kitchen.busy=sum(as.numeric(grepl("(cdf.R)",running.processes, ignore.case = T)))
if(kitchen.busy>2){
  
  print(paste(Sys.time(), ": kitchen is busy with order #",min(kitchen.log$ticket.number[kitchen.log$under.preparation==1]), sep=""))
  print(running.processes[grepl("(cdf.R)",running.processes, ignore.case = T)])
  
} else{
  
  
  ## setup
  # setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud/17 Shiny/4 data kitchen")
  # setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
  setwd("/home/rstudio/Dropbox/GTA cloud")
  source(paste0(path,"code/order processing/kitchen utensils.R"))
  load(paste0(path,"log/kitchen log.Rdata"))
  
  
  
  if(sum(kitchen.log$under.preparation)==0){
    print(paste(Sys.time(), ": no business", sep=""))
    
  }else{
    
    max.rnds=sum(kitchen.log$under.preparation)
    rnd=1
    ## if nothing is running, then start the oven.
    while(sum(kitchen.log$under.preparation)>0 & rnd<=max.rnds){
      
      # first come, first served.
      log.row=min(which(kitchen.log$under.preparation==1))
      
      logpath=paste(path,"results/",Sys.Date()," - GTA data dish #", kitchen.log$ticket.number[log.row],".txt",sep="")
      con <- file(logpath)
      sink(con, append=T)
      sink(con, append=T, type="message")
      
      ## Order: update GTA (clearing all outstanding update orders at once)
      if(kitchen.log$order.type[log.row]=="GTA"){
        kitchen.log$time.start[kitchen.log$order.type=="GTA" & kitchen.log$under.preparation==1]=Sys.time()
        class(kitchen.log$time.start)=c('POSIXt', 'POSIXct')
        save(kitchen.log, file=paste0(path,"log/kitchen log.Rdata"))
        
        collection(source(paste0(path,"code/order processing/GTA data update - 1 - data download.R"), echo=T, max.deparse.length=1000000, print.eval = T))
        collection(source(paste0(path,"code/order processing/GTA data update - 2 - database assembly.R"), echo=T, max.deparse.length=1000000, print.eval = T))
        collection(source(paste0(path,"code/order processing/GTA data update - 3 - master plus.R"), echo=T, max.deparse.length=1000000, print.eval = T))
        
        load(paste0(path,"log/kitchen log.Rdata"))
        kitchen.log$time.finish[kitchen.log$order.type=="GTA" & kitchen.log$under.preparation==1]=Sys.time()
        class(kitchen.log$time.finish)=c('POSIXt', 'POSIXct')
        kitchen.log$under.preparation[kitchen.log$order.type=="GTA" & kitchen.log$under.preparation==1]=0
      }
      
      
      
      ## Order: trade share
      if(kitchen.log$order.type[log.row]=="trade coverage"){
        kitchen.log$time.start[log.row]=Sys.time()
        class(kitchen.log$time.start)=c('POSIXt', 'POSIXct')
        save(kitchen.log, file=paste0(path,"log/kitchen log.Rdata"))
        ## extracting parameters
        kl=kitchen.log[log.row,]
        
        collection(source(paste0(path,"code/order processing/GTA data kitchen order - trade coverage.R"), echo=T, max.deparse.length=1000000, print.eval = T))
        
        if (error.message[1] == T) {
          
          ## Sending error email.
          library(mailR)
          sender = "data@globaltradealert.org"
          recipients = as.character(kitchen.log$order.email[log.row])
          
          sbjct=paste("Your order could not be processed [GTA data dish #",kitchen.log$ticket.number[log.row],"][",kitchen.log$order.name[log.row],"]",sep="")
          
          message=paste0("Hello \n\nThank you for your order. Your order encountered an error and could not be processed. \nThe error message was: ",error.message[2]," \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards \nGlobal Trade Alert Data")
          
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    # attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          send.mail(from = sender,
                    to = c("patrick.buess@student.unisg.ch","fritz.johannes@gmail.com"),
                    subject=paste0("[ERROR Data Kitchen]: ",sbjct),
                    body=message,
                    html=F,
                    # attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          ## updating log
          load(paste0(path,"log/kitchen log.Rdata"))
          kitchen.log$time.finish[log.row]=Sys.time()
          class(kitchen.log$time.finish)=c('POSIXt', 'POSIXct')
          kitchen.log$under.preparation[log.row]=0
          
          rm(recipients, message, sbjct, sender)
          
          
        }
        
        if (error.message[1]==F) {
          
          ## rename trade coverage file
          result.path=paste(path,"results/",Sys.Date()," - GTA data dish #", kitchen.log$ticket.number[log.row],".xlsx", sep="")
          # file.rename(paste(path,"results/GTA trade coverage estimates from ", Sys.Date(),".xlsx", sep=""),result.path)
          attachments <- c(result.path)
          
          ## attach interventions list if necessary
          if (kitchen.log$xlsx.interventions[log.row]) {
            results.path.interventions=paste(path,"results/",Sys.Date()," - GTA data dish #", kitchen.log$ticket.number[log.row]," - interventions.xlsx", sep="")
            # file.rename(paste(path,"results/GTA trade coverage interventions from ", Sys.Date(),".xlsx", sep=""),result.path)
            attachments <- c(attachments, results.path.interventions)
            
          }
          
          ## updating log
          load(paste0(path,"log/kitchen log.Rdata"))
          kitchen.log$time.finish[log.row]=Sys.time()
          class(kitchen.log$time.finish)=c('POSIXt', 'POSIXct')
          kitchen.log$under.preparation[log.row]=0
          
          
          ## Serving a fresh data dish.
          library(mailR)
          sender = "data@globaltradealert.org"
          recipients = as.character(kitchen.log$order.email[log.row])
          sbjct=paste("Your data is ready [GTA data dish #",kitchen.log$ticket.number[log.row],"][",kitchen.log$order.name[log.row],"]",sep="")
          message="Hello \n\nThank you for your order. Please find the requested GTA data in the attached Excel file. \n\nIn case of questions or suggestions, please reply to this message. \n\nRegards\nGlobal Trade Alert Data"
          
          
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          rm(recipients, message, sbjct, sender, attachments)
          
        }
      }
      
      
      
      ## Order: Count statistics
      if(kitchen.log$order.type[log.row]=="count statistics"){
        kitchen.log$time.start[log.row]=Sys.time()
        class(kitchen.log$time.start)=c('POSIXt', 'POSIXct')
        save(kitchen.log, file=paste0(path,"log/kitchen log.Rdata"))
        ## extracting parameters
        kl=kitchen.log[log.row,]
        
        source(paste0(path,"code/order processing/GTA data kitchen order - count statistics.R"), echo=T, max.deparse.length=1000000, print.eval = T)
        
        if (error.message[1] == T) {
          
          ## Sending error email.
          library(mailR)
          sender = "data@globaltradealert.org"
          recipients = as.character(kitchen.log$order.email[log.row])
          
          sbjct=paste("Your order could not be processed [GTA data dish #",kitchen.log$ticket.number[log.row],"][",kitchen.log$order.name[log.row],"]",sep="")
          
          message=paste0("Hello \n\nThank you for your order. Your order encountered an error and could not be processed. \nThe error message was: ",error.message[2]," \n\nIn case of questions or suggestions, please reply to this message.\n\nRegards\nGlobal Trade Alert Data")
          
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    # attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          send.mail(from = sender,
                    to = c("patrick.buess@student.unisg.ch","fritz.johannes@gmail.com"),
                    subject=paste0("[ERROR Data Kitchen]: ",sbjct),
                    body=message,
                    html=F,
                    # attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          ## updating log
          load(paste0(path,"log/kitchen log.Rdata"))
          kitchen.log$time.finish[log.row]=Sys.time()
          class(kitchen.log$time.finish)=c('POSIXt', 'POSIXct')
          kitchen.log$under.preparation[log.row]=0
          
          rm(recipients, message, sbjct, sender)
          
          
        }
        
        if (error.message[1]==F) {
          
          attachments <- c()
          
          ## rename data count xlsx file
          if (kl$xlsx == T) {
            result.path.xlsx=paste(path,"results/",Sys.Date()," - GTA data dish #", kitchen.log$ticket.number[log.row],".xlsx", sep="")
            file.rename(paste(path,"results/Count statistics from ", Sys.Date(),".xlsx", sep=""),result.path.xlsx)
            attachments <- c(attachments, result.path.xlsx)
            
          }
          
          
          ## rename count stat map
          if (kl$map == T) {
            result.path.map=paste(path,"results/",Sys.Date()," - GTA map #", kitchen.log$ticket.number[log.row],".png", sep="")
            file.rename(paste(path,"results/Map chart from ", Sys.Date(),".png", sep=""),result.path.map)
            attachments <- c(attachments, result.path.map)
            
          }
          
          ## rename count stat tile
          if (kl$tile == T) {
            result.path.tile=paste(path,"results/",Sys.Date()," - GTA tile #", kitchen.log$ticket.number[log.row],".png", sep="")
            file.rename(paste(path,"results/Tile chart from ", Sys.Date(),".png", sep=""),result.path.tile)
            attachments <- c(attachments, result.path.tile)
            
          }
          
          ## rename count stat line
          if (kl$line == T) {
            result.path.line=paste(path,"results/",Sys.Date()," - GTA line #", kitchen.log$ticket.number[log.row],".png", sep="")
            file.rename(paste(path,"results/Line chart from ", Sys.Date(),".png", sep=""),result.path.line)
            attachments <- c(attachments, result.path.line)
          }
          
          ## rename count stat bar
          if (kl$bar == T) {
            result.path.bar=paste(path,"results/",Sys.Date()," - GTA bar #", kitchen.log$ticket.number[log.row],".png", sep="")
            file.rename(paste(path,"results/Bar chart from ", Sys.Date(),".png", sep=""),result.path.bar)
            attachments <- c(attachments, result.path.bar)
          }
          
          ## rename count interventions list
          if (kl$interventions.list == T) {
            result.path.list=paste(path,"results/",Sys.Date()," -  GTA data dish # ", kitchen.log$ticket.number[log.row]," - list of interventions & parameter choices.xlsx", sep="")
            file.rename(paste(path,"results/Interventions list from ", Sys.Date(),".xlsx", sep=""),result.path.list)
            attachments <- c(attachments, result.path.list)
          }
          
          
          ## Serving a fresh data dish.
          sender = "data@globaltradealert.org"
          recipients = as.character(kitchen.log$order.email[log.row])
          sbjct=paste("Your data is ready [GTA data dish #",kitchen.log$ticket.number[log.row],"][",kitchen.log$order.name[log.row],"]",sep="")
          message="Hello \n\nThank you for your order. Please find the requested GTA data in the attached Excel file.\n\nIn case of questions or suggestions, please reply to this message.\n\nRegards\nGlobal Trade Alert Data"
          
          
          send.mail(from = sender,
                    to = recipients,
                    subject=sbjct,
                    body=message,
                    html=F,
                    attach.files = attachments,
                    smtp = list(host.name = "mail.infomaniak.com",
                                port=587,
                                user.name=sender,
                                passwd="B0d@nstrasse",
                                tls=T),
                    authenticate = T)
          
          ## updating log
          load(paste0(path,"log/kitchen log.Rdata"))
          kitchen.log$time.finish[log.row]=Sys.time()
          class(kitchen.log$time.finish)=c('POSIXt', 'POSIXct')
          kitchen.log$under.preparation[log.row]=0
          
          rm(recipients, message, sbjct, sender, attachments)
          
        }
        
      }
      
      ## Storing updated log
      save(kitchen.log, file = paste0(path,"log/kitchen log.Rdata"))
      load(paste0(path,"log/kitchen log.Rdata"))
      sink()
      sink(type="message")
      rnd=rnd+1
    }
  }
  
  
}


