
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
    if(grepl(": ", line, fixed = T) & grepl(" - ", line, fixed = T)){
      lspl = strsplit(line, " - ", fixed = T)[[1]]
      res[["datetime"]] = c(res[["datetime"]], lspl[1])
      res[["date"]] = c(res[["date"]], strsplit(lspl[1], ", ", fixed = T)[[1]][1])
      res[["time"]] = c(res[["time"]], strsplit(lspl[1], ", ", fixed = T)[[1]][2])
      res[["participant"]] = c(res[["participant"]], strsplit(lspl[2], ": ", fixed = T)[[1]][1])
      res[["message"]] = c(res[["message"]], strsplit(lspl[2], ": ", fixed = T)[[1]][2])
    } else if(!grepl(" - ", line, fixed = T)){
      res[["datetime"]] = c(res[["datetime"]], res[["datetime"]][length(res[["datetime"]])])
      res[["date"]] = c(res[["date"]], res[["date"]][length(res[["date"]])])
      res[["time"]] = c(res[["time"]], res[["time"]][length(res[["time"]])])
      res[["participant"]] = c(res[["participant"]], res[["participant"]][length(res[["participant"]])])
      res[["message"]] = c(res[["message"]], line)
    }
  }
  
  return(as.data.frame(res, stringsAsFactors = F))
}

# Prepare data
chat_data = readFile("data/20240526_Sobre merda.txt")
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

## fix date for Henrique's poops
### times are approximate, by indication of the participant
chat_data[chat_data$message=="💩 (o de ontem)", "datetime"] = lubridate::dmy_hm("16/12/23, 10:00")
chat_data[chat_data$message=="💩 (o de ontem)", "date"] = lubridate::dmy("16/12/23")
chat_data[chat_data$message=="💩 (o de ontem)", "time"] = lubridate::hm("10:00")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "datetime"] = lubridate::dmy_hm("15/12/23, 10:30")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "date"] = lubridate::dmy("15/12/23")
chat_data[chat_data$message=="💩 (o de sexta de manhã)", "time"] = lubridate::hm("10:30")
chat_data[chat_data$message=="💩 (1h atrás)", "datetime"] = lubridate::dmy_hm("07/01/24, 11:54")
chat_data[chat_data$message=="💩 (1h atrás)", "time"] = lubridate::hm("11:54")
chat_data[chat_data$message=="💩 (10:50h)", "datetime"] = lubridate::dmy_hm("03/01/24, 10:50")
chat_data[chat_data$message=="💩 (10:50h)", "time"] = lubridate::hm("10:50")
chat_data[chat_data$message=="💩(o de ontem às 10:30)", "datetime"] = lubridate::dmy_hm("08/01/24, 10:30")
chat_data[chat_data$message=="💩(o de ontem às 10:30)", "date"] = lubridate::dmy("08/01/24")
chat_data[chat_data$message=="💩(o de ontem às 10:30)", "time"] = lubridate::hm("10:30")
chat_data[chat_data$message=="💩(o de hoje às 11h)", "datetime"] = lubridate::dmy_hm("09/01/24, 11:00")
chat_data[chat_data$message=="💩(o de hoje às 11h)", "time"] = lubridate::hm("11:00")
chat_data[chat_data$message=="💩(o das 9 da manhã", "datetime"] = lubridate::dmy_hm("15/01/24, 9:00")
chat_data[chat_data$message=="💩(o das 9 da manhã", "time"] = lubridate::hm("9:00")
chat_data[chat_data$message=="💩(1:30 atrás)", "datetime"] = lubridate::dmy_hm("15/01/24, 16:42")
chat_data[chat_data$message=="💩(1:30 atrás)", "time"] = lubridate::hm("16:42")
chat_data[chat_data$message=="💩(das 8:50)", "datetime"] = lubridate::dmy_hm("18/01/24, 8:50")
chat_data[chat_data$message=="💩(das 8:50)", "time"] = lubridate::hm("8:50")
chat_data[chat_data$message=="💩 (das 9:45)", "datetime"] = lubridate::dmy_hm("19/01/24, 9:45")
chat_data[chat_data$message=="💩 (das 9:45)", "time"] = lubridate::hm("9:45")
chat_data[chat_data$message=="💩 (o das 10:45, esqueci-me 😅)", "datetime"] = lubridate::dmy_hm("04/02/24, 9:45")
chat_data[chat_data$message=="💩 (o das 10:45, esqueci-me 😅)", "time"] = lubridate::hm("9:45")
chat_data[chat_data$message=="💩 (o das 11h)" & chat_data$date=="2024/02/14", "datetime"] = lubridate::dmy_hm("14/02/24, 11:00")
chat_data[chat_data$message=="💩 (o das 11h)" & chat_data$date=="2024/02/14", "time"] = lubridate::hm("11:00")
chat_data[chat_data$message=="💩 (o das 17h)", "datetime"] = lubridate::dmy_hm("14/02/24, 17:00")
chat_data[chat_data$message=="💩 (o das 17h)", "time"] = lubridate::hm("17:00")
chat_data[chat_data$message=="💩 (meia-hora atrás)", "datetime"] = lubridate::dmy_hm("15/02/24, 9:37")
chat_data[chat_data$message=="💩 (meia-hora atrás)", "time"] = lubridate::hm("9:37")
chat_data[chat_data$message=="💩 (o das 11h)" & chat_data$date=="2024/02/25", "datetime"] = lubridate::dmy_hm("25/02/24, 11:00")
chat_data[chat_data$message=="💩 (o das 11h)" & chat_data$date=="2024/02/25", "time"] = lubridate::hm("11:00")
chat_data[chat_data$message=="💩 (o das 10:30 de ontem)", "datetime"] = lubridate::dmy_hm("04/03/2024, 10:30")
chat_data[chat_data$message=="💩 (o das 10:30 de ontem)", "date"] = lubridate::dmy("04/03/2024")
chat_data[chat_data$message=="💩 (o das 10:30 de ontem)", "time"] = lubridate::hm("10:30")
chat_data[chat_data$message=="💩 (o das 7:30h de hoje)", "datetime"] = lubridate::dmy_hm("05/03/2024, 7:30")
chat_data[chat_data$message=="💩 (o das 7:30h de hoje)", "time"] = lubridate::hm("7:30")

### adjust Henrique's time zone
chat_data[chat_data$participant=="Kicks","datetime"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","datetime"], "CET")
chat_data[chat_data$participant=="Kicks","time"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","time"], "CET")
chat_data[chat_data$participant=="Kicks","date"] = lubridate::with_tz(chat_data[chat_data$participant=="Kicks","date"], "CET")


## fix time for Escudeiro's poops
chat_data[chat_data$message=="💩 (retroactivo 13:30h)", "datetime"] = lubridate::dmy_hm("20/12/23, 13:30")
chat_data[chat_data$message=="💩 (retroactivo 13:30h)", "time"] = lubridate::hm("13:30")
chat_data[chat_data$message=="💩,06/01/2024,16:10", "datetime"] = lubridate::dmy_hm("06/01/24, 16:10")
chat_data[chat_data$message=="💩,06/01/2024,16:10", "time"] = lubridate::hm("16:10")

## fix time for Rui's poops
chat_data[chat_data$message=="💩 (foi por volta das 11:30)", "datetime"] = lubridate::dmy_hm("22/12/23, 11:30")
chat_data[chat_data$message=="💩 (foi por volta das 11:30)", "time"] = lubridate::hm("11:30")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "datetime"] = lubridate::dmy_hm("24/12/23, 23:00")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "date"] = lubridate::dmy("24/12/23")
chat_data[chat_data$message=="💩 (atrasado, foi por volta das 23h)", "time"] = lubridate::hm("23:00")
chat_data[chat_data$message=="💩 (foi as 10h) <Esta mensagem foi editada>", "datetime"] = lubridate::dmy_hm("07/01/24, 10:00")
chat_data[chat_data$message=="💩 (foi as 10h) <Esta mensagem foi editada>", "time"] = lubridate::hm("10:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="12/01/24", "datetime"] = lubridate::dmy_hm("12/01/24, 13:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="12/01/24", "time"] = lubridate::hm("13:00")
chat_data[chat_data$message=="💩( foi as 12h)", "datetime"] = lubridate::dmy_hm("15/01/24, 12:00")
chat_data[chat_data$message=="💩( foi as 12h)", "time"] = lubridate::hm("12:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="15/01/24", "datetime"] = lubridate::dmy_hm("15/01/24, 13:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="15/01/24", "time"] = lubridate::hm("13:00")
chat_data[chat_data$message=="💩 (foi as 17h)", "datetime"] = lubridate::dmy_hm("21/01/24, 17:00")
chat_data[chat_data$message=="💩 (foi as 17h)", "time"] = lubridate::hm("17:00")
chat_data[chat_data$message=="💩 (22h)", "datetime"] = lubridate::dmy_hm("23/01/24, 22:00")
chat_data[chat_data$message=="💩 (22h)", "time"] = lubridate::hm("22:00")
chat_data[chat_data$message=="💩 (foi as 12h)", "datetime"] = lubridate::dmy_hm("26/01/24, 12:00")
chat_data[chat_data$message=="💩 (foi as 12h)", "time"] = lubridate::hm("12:00")
chat_data[chat_data$message=="💩 (15h)", "datetime"] = lubridate::dmy_hm("02/02/24, 15:00")
chat_data[chat_data$message=="💩 (15h)", "time"] = lubridate::hm("15:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="2024-02-01", "datetime"] = lubridate::dmy_hm("01/02/24, 13:00")
chat_data[chat_data$message=="💩 (foi as 13h)" & chat_data$date=="2024-02-01", "time"] = lubridate::hm("13:00")
chat_data[chat_data$message=="💩 (foi as 15h)", "datetime"] = lubridate::dmy_hm("18/02/24, 15:00")
chat_data[chat_data$message=="💩 (foi as 15h)", "time"] = lubridate::hm("15:00")
chat_data[chat_data$message=="💩 (foi as 10h)", "datetime"] = lubridate::dmy_hm("18/02/24, 10:00")
chat_data[chat_data$message=="💩 (foi as 10h)", "time"] = lubridate::hm("10:00")
chat_data[chat_data$message=="💩 (11:30)", "datetime"] = lubridate::dmy_hm("08/03/24, 11:30")
chat_data[chat_data$message=="💩 (11:30)", "time"] = lubridate::hm("11:30")
chat_data[chat_data$message=="💩 (16:00)", "datetime"] = lubridate::dmy_hm("08/03/24, 16:00")
chat_data[chat_data$message=="💩 (16:00)", "time"] = lubridate::hm("16:00")
chat_data[chat_data$message=="💩 (14:30)", "datetime"] = lubridate::dmy_hm("04/03/24, 14:30")
chat_data[chat_data$message=="💩 (14:30)", "time"] = lubridate::hm("14:30")
chat_data[chat_data$message=="💩 (manhã)", "datetime"] = lubridate::dmy_hm("03/05/2024, 11:15")
chat_data[chat_data$message=="💩 (manhã)", "time"] = lubridate::hm("11:15")
chat_data[chat_data$message=="💩 (09:15)", "datetime"] = lubridate::dmy_hm("10/05/2024, 9:15")
chat_data[chat_data$message=="💩 (09:15)", "time"] = lubridate::hm("9:15")

# fix time for Tomas' poops
chat_data[chat_data$message=="💩,29/12/2023,22:50", "datetime"] = lubridate::dmy_hm("29/12/2023, 22:50")
chat_data[chat_data$message=="💩,29/12/2023,22:50", "date"] = lubridate::dmy("29/12/2023")
chat_data[chat_data$message=="💩,29/12/2023,22:50", "time"] = lubridate::hm("22:50")
chat_data[chat_data$message=="💩 (11:54) <Esta mensagem foi editada>", "datetime"] = lubridate::dmy_hm("19/04/2024, 11:54")
chat_data[chat_data$message=="💩 (11:54) <Esta mensagem foi editada>", "time"] = lubridate::hm("11:54")
chat_data[chat_data$message=="💩 (00:35 ontem)", "datetime"] = lubridate::dmy_hm("18/04/2024, 00:35")
chat_data[chat_data$message=="💩 (00:35 ontem)", "time"] = lubridate::hm("00:35")
chat_data[chat_data$message=="💩 (20:15)", "datetime"] = lubridate::dmy_hm("04/05/2024, 20:15")
chat_data[chat_data$message=="💩 (20:15)", "time"] = lubridate::hm("20:15")


# fix time for Cabecinha's poops
chat_data[chat_data$message=="💩(21h10)" & chat_data$date=="2024/01/13", "datetime"] = lubridate::dmy_hm("13/01/24, 21:10")
chat_data[chat_data$message=="💩(21h10)" & chat_data$date=="2024/01/13", "time"] = lubridate::hm("21:10")
chat_data[chat_data$message=="💩(20:45)", "datetime"] = lubridate::dmy_hm("25/01/24, 20:45")
chat_data[chat_data$message=="💩(20:45)", "time"] = lubridate::hm("20:45")
chat_data[chat_data$message=="💩02:22", "datetime"] = lubridate::dmy_hm("21/02/24, 02:20")
chat_data[chat_data$message=="💩02:22", "time"] = lubridate::hm("02:20")
chat_data[chat_data$message=="💩(ontem 22h30)", "datetime"] = lubridate::dmy_hm("04/03/2024, 22:30")
chat_data[chat_data$message=="💩(ontem 22h30)", "date"] = lubridate::dmy("04/03/2024")
chat_data[chat_data$message=="💩(ontem 22h30)", "time"] = lubridate::hm("22:30")
chat_data[chat_data$message=="💩 (00:10)" & chat_data$date=="2024/03/22", "datetime"] = lubridate::dmy_hm("22/03/24, 00:10")
chat_data[chat_data$message=="💩 (00:10)" & chat_data$date=="2024/03/22", "time"] = lubridate::hm("00:10")
chat_data[chat_data$message=="💩(20:05)", "datetime"] = lubridate::dmy_hm("23/04/24, 20:05")
chat_data[chat_data$message=="💩(20:05)", "time"] = lubridate::hm("20:05")
chat_data[chat_data$message=="💩(11h45)", "datetime"] = lubridate::dmy_hm("26/05/24, 11:45")
chat_data[chat_data$message=="💩(11h45)", "time"] = lubridate::hm("11:45")
chat_data[chat_data$message=="💩21h30", "datetime"] = lubridate::dmy_hm("22/05/24, 21:30")
chat_data[chat_data$message=="💩21h30", "time"] = lubridate::hm("21:30")
chat_data[chat_data$message=="💩(23h20)", "datetime"] = lubridate::dmy_hm("16/05/24, 23:20")
chat_data[chat_data$message=="💩(23h20)", "time"] = lubridate::hm("23:20")


# fix Canonico's poops
chat_data[chat_data$message=="💩 (8:30)", "datetime"] = lubridate::dmy_hm("26/02/24, 08:30")
chat_data[chat_data$message=="💩 (8:30)", "time"] = lubridate::hm("08:30")
chat_data[chat_data$message=="💩 (10:31)", "datetime"] = lubridate::dmy_hm("10/03/24, 10:31")
chat_data[chat_data$message=="💩 (10:31)", "time"] = lubridate::hm("10:31")
chat_data[chat_data$message=="💩(11:30)", "datetime"] = lubridate::dmy_hm("28/04/24, 11:30")
chat_data[chat_data$message=="💩(11:30)", "time"] = lubridate::hm("11:30")

# fix Tiago's poops
chat_data[chat_data$message=="💩 (dia 13 por volta das 14:00)", "datetime"] = lubridate::dmy_hm("13/03/2024, 14:00")
chat_data[chat_data$message=="💩 (dia 13 por volta das 14:00)", "date"] = lubridate::dmy("13/03/2024")
chat_data[chat_data$message=="💩 (dia 13 por volta das 14:00)", "time"] = lubridate::hm("14:00")
chat_data[chat_data$message=="💩 (13:00)", "datetime"] = lubridate::dmy_hm("27/03/24, 13:00")
chat_data[chat_data$message=="💩 (13:00)", "time"] = lubridate::hm("13:00")
chat_data[chat_data$message=="💩 (15:15)", "datetime"] = lubridate::dmy_hm("22/04/24, 15:15")
chat_data[chat_data$message=="💩 (15:15)", "time"] = lubridate::hm("15:15")
chat_data[chat_data$message=="💩 (16:30)", "datetime"] = lubridate::dmy_hm("12/05/24, 16:30")
chat_data[chat_data$message=="💩 (16:30)", "time"] = lubridate::hm("16:30")

# fix Andre's poops
chat_data[chat_data$message=="💩(18h00)", "datetime"] = lubridate::dmy_hm("20/04/2024, 18:00")
chat_data[chat_data$message=="💩(18h00)", "date"] = lubridate::dmy("20/04/2024")
chat_data[chat_data$message=="💩(18h00)", "time"] = lubridate::hm("18:00")
chat_data[chat_data$message=="💩(16h00)", "datetime"] = lubridate::dmy_hm("30/04/2024, 16:00")
chat_data[chat_data$message=="💩(16h00)", "time"] = lubridate::hm("16:00")


# remove one from Escudeiro
chat_data = chat_data[!grepl(pattern = "Pá é sexta à noite, decidi ver umas merdas n", 
                             chat_data$message),]
chat_data = unique(chat_data)

## only poops
poops_only = chat_data[grepl("\U0001f4a9", chat_data$message, fixed = T),]
## only chat
chat_only = chat_data[!grepl("\U0001f4a9", chat_data$message, fixed = T),]


