
# load libraries
library(ggplot2)
library(emoGG)
library(lubridate)
library(dplyr)

# important functions
## function to read lines
readFile = function(filepath) {
  l_l = list()
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1,encoding = "UTF-8")
    if ( length(line) == 0 ) {
      break
    }
    l_l[[length(l_l)+1]] = line
  }
  
  close(con)
  return(l_l)
}

## function to process lines
processLines = function(l){
  res = list("datetime" = c(),
             "date" = c(),
             "time" = c(),
             "participant" = c(),
             "message" = c())
  for(line in l){
    if(grepl(": ", line, fixed = T)){
      lspl = strsplit(line, " - ", fixed = T)[[1]]
      res[["datetime"]] = c(res[["datetime"]], lspl[1])
      res[["date"]] = c(res[["date"]], strsplit(lspl[1], ", ", fixed = T)[[1]][1])
      res[["time"]] = c(res[["time"]], strsplit(lspl[1], ", ", fixed = T)[[1]][2])
      res[["participant"]] = c(res[["participant"]], strsplit(lspl[2], ": ", fixed = T)[[1]][1])
      res[["message"]] = c(res[["message"]], strsplit(lspl[2], ": ", fixed = T)[[1]][2])
    }
  }
  
  return(as.data.frame(res, stringsAsFactors = F))
}

# Prepare data
chat_data = readFile("data/20240101_Sobre merda.txt")
chat_data = processLines(chat_data)

chat_data$datetime = lubridate::dmy_hm(chat_data$datetime)
chat_data$date = lubridate::dmy(chat_data$date)
chat_data$time = lubridate::hm(chat_data$time)

## rename participants
chat_data$participant[chat_data$participant=="Cabichona"] = "Cabecinha"
chat_data$participant[chat_data$participant=="Tomás Gomes"] = "Tomás"
chat_data$participant[chat_data$participant=="Tiago Pires"] = "Tiago"
chat_data$participant[chat_data$participant=="Ricardo Custódio"] = "Custódio"
chat_data$participant[chat_data$participant=="Kiks McKikings"] = "Kicks"
chat_data$participant[chat_data$participant=="Ruca mas Fixe"] = "Rui"
chat_data$participant[chat_data$participant=="Canonz10Tiago2"] = "Canónico"
chat_data$participant[chat_data$participant=="AndréTheRockTyson"] = "André"
chat_data$participant[chat_data$participant=="Herpetologo WannaB"] = "Escudeiro"
chat_data$participant = factor(chat_data$participant)

## starts on 15/12/2023
start_date = chat_data$date>lubridate::dmy("14/12/23")
chat_data = chat_data[start_date,]

## remove my bad poops
bad_poops = chat_data$time > lubridate::hm("2:36") & 
  chat_data$time < lubridate::hm("2:44") & 
  chat_data$date==lubridate::dmy("15/12/23")
chat_data = chat_data[!bad_poops,]

## fix date for Henrique's first three poops
### times are approximate, by indication of the participant
chat_data[chat_data$message=="💩 (o de ontem)", "datetime"] = lubridate::dmy_hm("16/12/23, 10:00")
chat_data[chat_data$message=="💩 (o de ontem)", "date"] = lubridate::dmy("16/12/23")
chat_data[chat_data$message=="💩 (o de ontem)", "time"] = lubridate::hm("10:00")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "datetime"] = lubridate::dmy_hm("15/12/23, 10:30")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "date"] = lubridate::dmy("15/12/23")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "time"] = lubridate::hm("10:30")
### adjust Henrique's time zone
chat_data[chat_data$participant=="Kicks","datetime"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","datetime"], "CET")
chat_data[chat_data$participant=="Kicks","time"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","time"], "CET")
chat_data[chat_data$participant=="Kicks","date"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","date"], "CET")


## fix time for Escudeiro's poops
chat_data[chat_data$message=="💩 (retroactivo 13:30h)", "datetime"] = lubridate::dmy_hm("20/12/23, 13:30")
chat_data[chat_data$message=="💩 (retroactivo 13:30h)", "time"] = lubridate::hm("13:30")

## fix time for Rui's poops
chat_data[chat_data$message=="💩 (foi por volta das 11:30)", "datetime"] = lubridate::dmy_hm("22/12/23, 11:30")
chat_data[chat_data$message=="💩 (foi por volta das 11:30)", "time"] = lubridate::hm("11:30")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "datetime"] = lubridate::dmy_hm("24/12/23, 23:00")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "date"] = lubridate::dmy("24/12/23")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "time"] = lubridate::hm("23:00")

# fix time for Tomas' poops
chat_data[chat_data$message=="💩,29/12/2023,22:50", "datetime"] = lubridate::dmy_hm("29/12/2023, 22:50")
chat_data[chat_data$message=="💩,29/12/2023,22:50", "date"] = lubridate::dmy("29/12/2023")
chat_data[chat_data$message=="💩,29/12/2023,22:50", "time"] = lubridate::hm("22:50")



## only poops
poops_only = chat_data[grepl("\U0001f4a9", chat_data$message, fixed = T),]
## only chat
chat_only = chat_data[!grepl("\U0001f4a9", chat_data$message, fixed = T),]


