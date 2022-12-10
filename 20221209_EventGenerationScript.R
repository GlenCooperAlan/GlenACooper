---
  title: "Event Data"
output: 
  flexdashboard::flex_dashboard:
  orientation: columns
social: menu
source_code: embed 
runtime: shiny

---
  
  ### Event Data
  
  ```{r global, include=FALSE}
library(flexdashboard)
library(shiny)
library(dplyr)
library(jsonlite)
library(jsonify)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(plotly)

nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

event_data <- fromJSON("https://raw.githubusercontent.com/kiranjaura/DataViz/main/ASEAN_Eventdata.json") %>% as.data.frame


names(event_data)<-c("Id","Province","Publisher", "TargetName",    "District","City", "Country","CAMEOCode","SourceCountry","SourceSectors","EventDate","Source Name","Latitude","Intensity"     ,"StoryID","TargetSectors","EventText","Longitude","TargetCountry","SentenceNumber","EventID","EventDateVal")

event_data <- subset(event_data, SourceCountry %in% c("China","Cambodia","Brunei","Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam") | TargetCountry %in% c("China","Cambodia","Brunei","Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"))


event_data$RootCode<-substr(event_data$CAMEOCode, 1, 2)
event_data$Year<-substr(event_data$EventDate, 1, 4)
#new_date <- epidate(event_data$EventDate, format = "%m-%d-%y")
#event_data$Month<-new_date$month
event_data$Month<-as.numeric(substr(event_data$EventDate, 6, 7))

event_data <- event_data %>% plyr::mutate(MonthName = dplyr::case_when(Month ==1 ~ "January",
                                                                       Month == 2 ~ "February",
                                                                       Month == 3 ~ "March",
                                                                       Month == 4 ~ "April",
                                                                       Month == 5 ~ "May",
                                                                       Month == 6 ~ "June",
                                                                       Month == 7 ~ "July",
                                                                       Month == 8 ~ "August",
                                                                       Month == 9 ~ "September",
                                                                       Month == 10 ~ "October",
                                                                       Month == 11 ~ "November",
                                                                       Month == 12 ~ "December"))

event_data$EventDate <- ymd(event_data$EventDate)



event_data$EventCount <- 1
event_data$RootLabel <- as.factor(ifelse(event_data$RootCode == "01", "MAKE PUBLIC STATEMENT",
                                         ifelse(event_data$RootCode == "02", "APPEAL", 
                                                ifelse(event_data$RootCode == "03", "EXPRESS INTENT TO COOPERATE",
                                                       ifelse(event_data$RootCode == "04", "CONSULT",
                                                              ifelse(event_data$RootCode == "05", "ENGAGE IN DIPLOMATIC COOPERATION",
                                                                     ifelse(event_data$RootCode == "06", "ENGAGE IN MATERIAL COOPERATION",
                                                                            ifelse(event_data$RootCode == "07", "PROVIDE AID",
                                                                                   ifelse(event_data$RootCode == "08", "YIELD",
                                                                                          ifelse(event_data$RootCode == "09", "INVESTIGATE",
                                                                                                 ifelse(event_data$RootCode == "10", "DEMAND",
                                                                                                        ifelse(event_data$RootCode == "11",  "DISAPPROVE",
                                                                                                               ifelse(event_data$RootCode == "12", "REJECT",
                                                                                                                      ifelse(event_data$RootCode == "13",  "THREATEN",
                                                                                                                             ifelse(event_data$RootCode == "14",  "PROTEST",
                                                                                                                                    ifelse(event_data$RootCode == "15", "EXHIBIT FORCE POSTURE",
                                                                                                                                           ifelse(event_data$RootCode == "16", "REDUCE RELATIONS",
                                                                                                                                                  ifelse(event_data$RootCode == "17" , "COERCE",
                                                                                                                                                         ifelse(event_data$RootCode == "18", "ASSAULT",
                                                                                                                                                                ifelse(event_data$RootCode == "19", "FIGHT",
                                                                                                                                                                       ifelse(event_data$RootCode == "20", "USE UNCONVENTIONAL MASS VIOLENCE",0)))))))))))))))))))))

event_data <- event_data %>% plyr::mutate(CLabel = dplyr::case_when(CAMEOCode == "010" ~ "Make statement",
                                                                    CAMEOCode=="011" ~ "Decline comment", 
                                                                    CAMEOCode == "012" ~ "Make pessimistic comment",
                                                                    CAMEOCode == "013" ~ "Make optimistic comment", 
                                                                    CAMEOCode == "014" ~ "Consider policy option", 
                                                                    CAMEOCode == "015"~"Acknowledge or claim responsibility",
                                                                    CAMEOCode == "016"~"Deny responsibilityy",
                                                                    CAMEOCode == "017"~ "Engage in symbolic act",
                                                                    CAMEOCode == "018"~ "Make empathetic comment",
                                                                    CAMEOCode == "019"~ "Express accord", 
                                                                    CAMEOCode == "020"~ "Make an appeal or request",
                                                                    CAMEOCode == "021"~ "Appeal for material cooperation",
                                                                    CAMEOCode == "0211"~ "Appeal for economic cooperation", 
                                                                    CAMEOCode == "0212"~ "Appeal for military cooperation", 
                                                                    CAMEOCode == "0213"~ "Appeal for judicial cooperation", 
                                                                    CAMEOCode == "0214"~ "Appeal for intelligence", 
                                                                    CAMEOCode == "022"~ "Appeal for diplomatic cooperation (such as policy support)", 
                                                                    CAMEOCode == "023"~ "Appeal for aid", 
                                                                    CAMEOCode == "0231"~ "Appeal for economic aid", 
                                                                    CAMEOCode == "0232"~ "Appeal for military aid", 
                                                                    CAMEOCode == "0233"~ "Appeal for humanitarian aid",
                                                                    CAMEOCode == "0234"~ "Appeal for military protection or peacekeeping", 
                                                                    CAMEOCode == "024"~ "Appeal for political reform",  
                                                                    CAMEOCode == "0241"~ "Appeal for change in leadership", 
                                                                    CAMEOCode == "0242"~ "Appeal for policy change", 
                                                                    CAMEOCode == "0243"~ "Appeal for rights", 
                                                                    CAMEOCode == "0244"~ "Appeal for change in institutions, regime", 
                                                                    CAMEOCode == "025"~ "Appeal to yield", 
                                                                    CAMEOCode == "0251"~ "Appeal for easing of administrative sanctions",
                                                                    CAMEOCode == "0252"~ "Appeal for easing of political dissent", 
                                                                    CAMEOCode == "0253"~ "Appeal for release of persons or property", 
                                                                    CAMEOCode == "0254"~ "Appeal for easing of economic sanctions, boycott, or embargo", 
                                                                    CAMEOCode == "0255"~ "Appeal for target to allow international involvement (non-mediation)", 
                                                                    CAMEOCode == "0256"~ "Appeal for de-escalation of military engagement",
                                                                    CAMEOCode == "026"~ "Appeal to others to meet or negotiate", 
                                                                    CAMEOCode == "027"~ "Appeal to others to settle dispute",
                                                                    CAMEOCode == "028"~ "Appeal to engage in or accept mediation", 
                                                                    CAMEOCode == "030"~ "Express intent to cooperate", 
                                                                    CAMEOCode == "031"~ "Express intent to engage in material cooperation", 
                                                                    CAMEOCode == "0311"~ "Express intent to cooperate economically", 
                                                                    CAMEOCode == "0312"~ "Express intent to cooperate militarily", 
                                                                    CAMEOCode == "0313"~ "Express intent to cooperate on judicial matters", 
                                                                    CAMEOCode == "0314"~ "Express intent to cooperate on intelligence", 
                                                                    CAMEOCode == "032"~ "Express intent to engage in diplomatic cooperation (such as policy support)", 
                                                                    CAMEOCode == "033"~ "Express intent to provide material aid", 
                                                                    CAMEOCode == "0331"~ "Express intent to provide economic aid", 
                                                                    CAMEOCode == "0332"~ "Express intent to provide military aid", 
                                                                    CAMEOCode == "0333"~ "Express intent to provide humanitarian aid", 
                                                                    CAMEOCode == "0334"~ "Express intent to provide military protection or peacekeeping", 
                                                                    CAMEOCode == "034"~ "Express intent to institute political reform", 
                                                                    CAMEOCode == "0341"~ "Express intent to change leadership",
                                                                    CAMEOCode == "0342"~ "Express intent to change policy", 
                                                                    CAMEOCode == "343"~ "Express intent to provide rights", 
                                                                    CAMEOCode == "0344"~ "Express intent to change institutions, regime", CAMEOCode == "035"~ "Express intent to yield", 
                                                                    CAMEOCode == "0351"~ "Express intent to ease administrative sanctions",
                                                                    CAMEOCode == "0352"~ "Express intent to ease popular dissent", 
                                                                    CAMEOCode == "0353"~ "Express intent to release persons or property", CAMEOCode == "0354"~ "Express intent to ease economic sanctions, boycott, or embargo", 
                                                                    CAMEOCode == "0355"~ "Express intent to allow international involvement (non-mediation)", 
                                                                    CAMEOCode == "0356"~ "Express intent to de-escalate military engagement", 
                                                                    CAMEOCode == "036"~ "Express intent to meet or negotiate", 
                                                                    CAMEOCode == "037"~ "Express intent to settle dispute", 
                                                                    CAMEOCode == "038"~ "Express intent to accept mediation", 
                                                                    CAMEOCode == "039"~ "Express intent to mediate", 
                                                                    CAMEOCode == "040"~ "Consult", 
                                                                    CAMEOCode == "041"~ "Discuss by telephone",
                                                                    CAMEOCode == "042"~ "Make a visit", 
                                                                    CAMEOCode == "043"~ "Host a visit", 
                                                                    CAMEOCode == "044"~ "Meet at a "third" location", 
                                                                    CAMEOCode == "045"~ "Mediate", 
                                                                    CAMEOCode == "046"~ "Engage in negotiation", 
                                                                    CAMEOCode == "050"~ "Engage in diplomatic cooperation", 
                                                                    CAMEOCode == "051"~ "Praise or endorse",
                                                                    CAMEOCode == "052"~ "Defend verbally", 
                                                                    CAMEOCode == "053"~ "Rally support on behalf of",
                                                                    CAMEOCode == "054"~ "Grant diplomatic recognition",
                                                                    CAMEOCode == "055"~ "Apologize",
                                                                    CAMEOCode == "056" ~ "Forgive",
                                                                    CAMEOCode == "057"~ "Sign formal agreement",
                                                                    CAMEOCode == "060"~ "Engage in material cooperation",
                                                                    CAMEOCode == "061"~ "Cooperate economically",
                                                                    CAMEOCode == "062"~ "Cooperate militarily", 
                                                                    CAMEOCode == "063"~ "Engage in judicial cooperation", 
                                                                    CAMEOCode == "064"~ "Share intelligence or information", 
                                                                    CAMEOCode == "070"~ "Provide aid",
                                                                    CAMEOCode == "071"~ "Provide economic aid", 
                                                                    CAMEOCode == "072"~ "Provide military aid", 
                                                                    CAMEOCode == "073"~ "Provide humanitarian aid", 
                                                                    CAMEOCode == "074"~ "Provide military protection or peacekeeping", 
                                                                    CAMEOCode == "075"~ "Grant asylum", 
                                                                    CAMEOCode == "080"~ "Yield", 
                                                                    CAMEOCode == "081"~ "Ease administrative sanctions", 
                                                                    CAMEOCode == "0811"~ "Ease restrictions on political freedoms", 
                                                                    CAMEOCode == "0812"~ "Ease ban on political parties or politicians", 
                                                                    CAMEOCode == "0813"~ "Ease curfew",
                                                                    CAMEOCode == "0814"~ "Ease state of emergency or martial law", 
                                                                    CAMEOCode == "082"~ "Ease political dissent", 
                                                                    CAMEOCode == "083"~ "Accede to requests or demands for political reform", 
                                                                    CAMEOCode == "0831"~ "Accede to demands for change in leadership", 
                                                                    CAMEOCode == "0832"~ "Accede to demands for change in policy", 
                                                                    CAMEOCode == "0833"~ "Accede to demands for rights", 
                                                                    CAMEOCode == "0834"~ "Accede to demands for change in institutions, regime", 
                                                                    CAMEOCode == "084"~ "Return, release", 
                                                                    CAMEOCode == "0841"~ "Return, release person(s)",
                                                                    CAMEOCode == "0842"~ "Return, release property", 
                                                                    CAMEOCode == "085"~ "Ease economic sanctions, boycott, embargo", 
                                                                    CAMEOCode == "086"~ "Allow international involvement", 
                                                                    CAMEOCode == "0861"~ "Receive deployment of peacekeepers",
                                                                    CAMEOCode == "0862"~ "Receive inspectors", 
                                                                    CAMEOCode == "0863"~ "Allow humanitarian access", 
                                                                    CAMEOCode == "087"~ "De-escalate military engagement", 
                                                                    CAMEOCode == "0871"~ "Declare truce, ceasefire", 
                                                                    CAMEOCode == "0872"~ "Ease military blockade",
                                                                    CAMEOCode == "0873"~ "Demobilize armed forces", 
                                                                    CAMEOCode == "0874"~ "Retreat or surrender militarily", 
                                                                    CAMEOCode == "090"~ "Investigate", 
                                                                    CAMEOCode == "091"~ "Investigate crime, corruption", 
                                                                    CAMEOCode == "092"~ "Investigate human rights abuses", 
                                                                    CAMEOCode == "093"~ "Investigate military action", 
                                                                    CAMEOCode == "094"~ "Investigate war crime", 
                                                                    CAMEOCode == "100"~ "Demand", 
                                                                    CAMEOCode == "101"~ "Demand material cooperation", 
                                                                    CAMEOCode == "1011"~ "Demand economic cooperation", 
                                                                    CAMEOCode == "1012"~ "Demand military cooperation", 
                                                                    CAMEOCode == "1013"~ "Demand judicial cooperation", 
                                                                    CAMEOCode == "1014"~ "Demand intelligence cooperation", 
                                                                    CAMEOCode == "102"~ "Demand diplomatic cooperation (such as policy support)", 
                                                                    CAMEOCode == "103"~ "Demand material aid", 
                                                                    CAMEOCode == "1031"~ "Demand economic aid", 
                                                                    CAMEOCode == "1032"~ "Demand military aid",  
                                                                    CAMEOCode == "1033"~ "Demand humanitarian aid", 
                                                                    CAMEOCode == "1034"~ "Demand military protection or peacekeeping", 
                                                                    CAMEOCode == "104"~ "Demand political reform", 
                                                                    CAMEOCode == "1041"~ "Demand change in leadership", 
                                                                    CAMEOCode == "1042"~ "Demand policy change", 
                                                                    CAMEOCode == "1043"~ "Demand rights", 
                                                                    CAMEOCode == "1044"~ "Demand change in institutions, regime", 
                                                                    CAMEOCode == "105"~ "Demand that target yields", 
                                                                    CAMEOCode == "1051"~ "Demand easing of administrative sanctions", 
                                                                    CAMEOCode == "1052"~ "Demand easing of political dissent",
                                                                    CAMEOCode == "1053"~ "Demand release of persons or property", 
                                                                    CAMEOCode == "1054"~ "Demand easing of economic sanctions, boycott, or embargo", 
                                                                    CAMEOCode == "1055"~ "Demand that target allows international involvement (non-mediation)", 
                                                                    CAMEOCode == "1056"~ "Demand de-escalation of military engagement", 
                                                                    CAMEOCode == "106"~ "Demand meeting, negotiation", 
                                                                    CAMEOCode == "107"~ "Demand settling of dispute", 
                                                                    CAMEOCode == "108"~ "Demand mediation", 
                                                                    CAMEOCode == "110"~ "Disapprove", 
                                                                    CAMEOCode == "111"~ "Criticize or denounce", 
                                                                    CAMEOCode == "112"~ "Accuse", 
                                                                    CAMEOCode == "1121"~ "Accuse of crime, corruption", 
                                                                    CAMEOCode == "1122"~ "Accuse of human rights abuses",
                                                                    CAMEOCode == "1123"~ "Accuse of aggression", 
                                                                    CAMEOCode == "1124"~ "Accuse of war crimes", 
                                                                    CAMEOCode == "1125"~ "Accuse of espionage, treason", 
                                                                    CAMEOCode == "113"~ "Rally opposition against",   
                                                                    CAMEOCode == "114"~ "Complain officially", 
                                                                    CAMEOCode == "115"~ "Bring lawsuit against", 
                                                                    CAMEOCode == "116"~ "Find guilty or liable (legally)", 
                                                                    CAMEOCode == "120"~ "Reject", 
                                                                    CAMEOCode == "121"~ "Reject material cooperation", 
                                                                    CAMEOCode == "1211"~ "Reject economic cooperation", 
                                                                    CAMEOCode == "1212"~ "Reject military cooperation", 
                                                                    CAMEOCode == "122"~ "Reject request or demand for material aid", 
                                                                    CAMEOCode == "1221"~ "Reject request for economic aid", 
                                                                    CAMEOCode == "1222"~ "Reject request for military aid", 
                                                                    CAMEOCode == "1223"~ "Reject request for humanitarian aid",
                                                                    CAMEOCode == "1224"~ "Reject request for military protection or peacekeeping", 
                                                                    CAMEOCode == "123"~ "Reject request or demand for political reform", 
                                                                    CAMEOCode == "1231"~ "Reject request for change in leadership", 
                                                                    CAMEOCode == "1232"~ "Reject request for policy change", 
                                                                    CAMEOCode == "1233"~ "Reject request for rights", 
                                                                    CAMEOCode == "1234"~ "Reject request for change in institutions, regime", 
                                                                    CAMEOCode == "124"~ "Refuse to yield", 
                                                                    CAMEOCode == "1241"~ "Refuse to ease administrative sanctions", 
                                                                    CAMEOCode == "1242"~ "Refuse to ease popular dissent", 
                                                                    CAMEOCode == "1243"~ "Refuse to release persons or property", 
                                                                    CAMEOCode == "1244"~ "Refuse to ease economic sanctions, boycott, or embargo", 
                                                                    CAMEOCode == "1245"~ "Refuse to allow international involvement (non mediation)", 
                                                                    CAMEOCode == "1246"~ "Refuse to de-escalate military engagement", 
                                                                    CAMEOCode == "125"~ "Reject proposal to meet, discuss, or negotiate", 
                                                                    CAMEOCode == "126"~ "Reject mediation", 
                                                                    CAMEOCode == "127"~ "Reject plan, agreement to settle dispute", 
                                                                    CAMEOCode == "128"~ "Defy norms, law", 
                                                                    CAMEOCode == "129"~ "Veto", 
                                                                    CAMEOCode == "130"~ "Threaten", 
                                                                    CAMEOCode == "131"~ "Threaten non-force", 
                                                                    CAMEOCode == "1311"~ "Threaten to reduce or stop aid", 
                                                                    CAMEOCode == "1312"~ "Threaten with sanctions, boycott, embargo", 
                                                                    CAMEOCode == "1313"~ "Threaten to reduce or break relations", 
                                                                    CAMEOCode == "132"~ "Threaten with administrative sanctions", 
                                                                    CAMEOCode == "1321"~ "Threaten with restrictions on political freedoms", 
                                                                    CAMEOCode == "1322"~ "Threaten to ban political parties or politicians", 
                                                                    CAMEOCode == "1323"~ "Threaten to impose curfew", 
                                                                    CAMEOCode == "1324"~ "Threaten to impose state of emergency or martial law", 
                                                                    CAMEOCode == "133"~ "Threaten with political dissent, protest", 
                                                                    CAMEOCode == "134"~ "Threaten to halt negotiations", 
                                                                    CAMEOCode == "135"~ "Threaten to halt mediation", 
                                                                    CAMEOCode == "136"~ "Threaten to halt international involvement (non-mediation)", 
                                                                    CAMEOCode == "137"~ "Threaten with repression", 
                                                                    CAMEOCode == "138"~ "Threaten with military force", 
                                                                    CAMEOCode == "1381"~ "Threaten blockade", 
                                                                    CAMEOCode == "1382"~ "Threaten occupation", 
                                                                    CAMEOCode == "1383"~ "Threaten unconventional violence", 
                                                                    CAMEOCode == "1384"~ "Threaten conventional attack", 
                                                                    CAMEOCode == "1385"~ "Threaten attack with WMD", 
                                                                    CAMEOCode == "139"~ "Give ultimatum", 
                                                                    CAMEOCode == "140"~ "Engage in political dissent", 
                                                                    CAMEOCode == "141"~ "Demonstrate or rally", 
                                                                    CAMEOCode == "1411"~ "Demonstrate for leadership change", 
                                                                    CAMEOCode == "1412"~ "Demonstrate for policy change", 
                                                                    CAMEOCode == "1413"~ "Demonstrate for rights", 
                                                                    CAMEOCode == "1414"~ "Demonstrate for change in institutions, regime", 
                                                                    CAMEOCode == "142"~ "Conduct hunger strike", 
                                                                    CAMEOCode == "1421"~ "Conduct hunger strike for leadership change", 
                                                                    CAMEOCode == "1422"~ "Conduct hunger strike for policy change", 
                                                                    CAMEOCode == "1423"~ "Conduct hunger strike for rights", 
                                                                    CAMEOCode == "1424"~ "Conduct hunger strike for change in institutions, regime", 
                                                                    CAMEOCode == "143"~ "Conduct strike or boycott", 
                                                                    CAMEOCode == "1431"~ "Conduct strike or boycott for leadership change", 
                                                                    CAMEOCode == "1432"~ "Conduct strike or boycott for policy change", 
                                                                    CAMEOCode == "1433"~ "Conduct strike or boycott for rights",
                                                                    CAMEOCode == "1434"~ "Conduct strike or boycott for change in institutions, regime",
                                                                    CAMEOCode == "144"~ "Obstruct passage, block", 
                                                                    CAMEOCode == "1441"~ "Obstruct passage to demand leadership change",
                                                                    CAMEOCode == "1442"~ "Obstruct passage to demand policy change", 
                                                                    CAMEOCode == "1443"~ "Obstruct passage to demand rights", 
                                                                    CAMEOCode == "1444"~ "Obstruct passage to demand change in institutions, regime", 
                                                                    CAMEOCode == "145"~ "Protest violently, riot", 
                                                                    CAMEOCode == "1451"~ "Engage in violent protest for leadership change", 
                                                                    CAMEOCode == "1452"~ "Engage in violent protest for policy change", 
                                                                    CAMEOCode == "1453"~ "Engage in violent protest for rights", 
                                                                    CAMEOCode == "1454"~ "Engage in violent protest for change in institutions, regime", 
                                                                    CAMEOCode == "150"~ "Demonstrate military or police power",
                                                                    CAMEOCode == "151"~ "Increase police alert status", 
                                                                    CAMEOCode == "152"~ "Increase military alert status", 
                                                                    CAMEOCode == "153"~ "Mobilize or increase police power", 
                                                                    CAMEOCode == "154"~ "Mobilize or increase armed forces", 
                                                                    CAMEOCode == "155"~ "Mobilize or increase cyber-force", 
                                                                    CAMEOCode == "160"~ "Reduce relations", 
                                                                    CAMEOCode == "161"~ "Reduce or break diplomatic relations",
                                                                    CAMEOCode == "162"~ "Reduce or stop material aid", 
                                                                    CAMEOCode == "1621"~ "Reduce or stop economic assistance",
                                                                    CAMEOCode == "1622"~ "Reduce or stop military assistance",
                                                                    CAMEOCode == "1623"~ "Reduce or stop humanitarian assistance",
                                                                    CAMEOCode == "163"~ "Impose embargo, boycott, or sanctions", 
                                                                    CAMEOCode == "164"~ "Halt negotiations", 
                                                                    CAMEOCode == "165"~ "Halt mediation", 
                                                                    CAMEOCode == "166"~ "Expel or withdraw", 
                                                                    CAMEOCode == "1661"~ "Expel or withdraw peacekeepers", 
                                                                    CAMEOCode == "1662"~ "Expel or withdraw inspectors, observers",
                                                                    CAMEOCode == "170"~ "Coerce",
                                                                    CAMEOCode == "171"~ "Seize or damage property", 
                                                                    CAMEOCode == "1711"~ "Confiscate property",
                                                                    CAMEOCode == "1712"~ "Destroy property", 
                                                                    CAMEOCode == "172"~ "Impose administrative sanctions", 
                                                                    CAMEOCode == "1721"~ "Impose restrictions on political freedoms",
                                                                    CAMEOCode == "1722"~ "Ban political parties or politicians",
                                                                    CAMEOCode == "1723"~ "Impose curfew",
                                                                    CAMEOCode == "1724"~ "Impose state of emergency or martial law", CAMEOCode == "173"~ "Arrest, detain, or charge with legal action",
                                                                    CAMEOCode == "174"~ "Expel or deport individuals", 
                                                                    CAMEOCode == "175"~ "Use tactics of violent repression", 
                                                                    CAMEOCode == "176"~ "Attack cybernetically", 
                                                                    CAMEOCode == "180"~ "Use unconventional violence",
                                                                    CAMEOCode == "181"~ "Abduct, hijack, or take hostage", 
                                                                    CAMEOCode == "182"~ "Physically assault", 
                                                                    CAMEOCode == "1821"~ "Sexually assault", 
                                                                    CAMEOCode == "1822"~ "Torture", 
                                                                    CAMEOCode == "1823"~ "Kill by physical assault", 
                                                                    CAMEOCode == "183"~ "Conduct suicide, car, or other non-military bombing",
                                                                    CAMEOCode == "1831"~ "Carry out suicide bombing", 
                                                                    CAMEOCode == "1832"~ "Carry out vehicular bombing", 
                                                                    CAMEOCode == "1833"~ "Carry out roadside bombing", 
                                                                    CAMEOCode == "1834"~ "Carry out location bombing", 
                                                                    CAMEOCode == "184"~ "Use as human shield", 
                                                                    CAMEOCode == "185"~ "Attempt to assassinate", 
                                                                    CAMEOCode == "186"~ "Assassinate",
                                                                    CAMEOCode == "190"~ "Use conventional military force", 
                                                                    CAMEOCode == "191"~ "Impose blockade, restrict movement", 
                                                                    CAMEOCode == "192"~ "Occupy territory", 
                                                                    CAMEOCode == "193"~ "Fight with small arms and light weapons",
                                                                    CAMEOCode == "194"~ "Fight with artillery and tanks", 
                                                                    CAMEOCode == "195"~ "Employ aerial weapons", 
                                                                    CAMEOCode == "1951"~ "Employ precision-guided aerial munitions",
                                                                    CAMEOCode == "1952"~ "Employ remotely piloted aerial munitions",
                                                                    CAMEOCode == "196"~ "Violate ceasefire", 
                                                                    CAMEOCode == "200"~ "Use unconventional mass violence", 
                                                                    CAMEOCode == "201"~ "Engage in mass expulsion", 
                                                                    CAMEOCode == "202"~ "Engage in mass killings", 
                                                                    CAMEOCode == "203"~ "Engage in ethnic cleansing", 
                                                                    CAMEOCode == "204"~ "Use weapons of mass destruction", 
                                                                    CAMEOCode == "2041"~ "Use chemical, biological, or radiological weapons",
                                                                    CAMEOCode == "2042"~ "Detonate nuclear weapon", 
                                                                    TRUE ~ CAMEOCode))

event_data$CLabel <- as.factor(event_data$CLabel)

event_data$Root <- paste(event_data$RootCode,":",event_data$RootLabel)

event_data$CAMEO <- paste(event_data$CAMEOCode,":",event_data$CLabel)

event_data <- event_data %>% plyr::mutate(Status = dplyr::case_when(RootCode %in% c("01","02") ~ "Neutral",
                                                                    RootCode %in% c("03","04","05","06","07","08") ~ "Cooperation",
                                                                    RootCode %in% c("09","10","11","12","13","14","15","16","17","18","19","20") ~ "Conflict"))

event_data$Status<-as.factor(event_data$Status)
```

Selections {.sidebar}
=====================================
  
  ```{r echo=FALSE, message=FALSE}
selectInput(inputId = "countrynamsource",
            label = "Source country:",
            choices = c("China","Cambodia","Brunei","Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"))
selectInput(inputId = "countrynamtarget",
            label = "Target country:",
            choices = c("Cambodia","China","Brunei", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam"))
selectInput(inputId = "year",
            label = "CAMEO Code Year Selection:",
            choices = rep(2012:2020,1))
selectInput(inputId = "month",
            label = "CAMEO Code Month Selection:",
            choices = c("January","February","March","April","May","June","July","August","September","October","November","December"))

```

Events Timeline
=====================================  
  ```{r}

mydata2 <- reactive({
  conam1 <- input$countrynamsource
  conam2 <- input$countrynamtarget
  Rd_data1 <- subset(event_data, SourceCountry %in% conam1)
  Rd_data2 <- subset(Rd_data1, TargetCountry %in% conam2)
})


renderPlotly({
  shiny::validate(
    need(nrow(mydata2()) > 0, "No data for this selection.")
  )
  md2 <- mydata2()
  options(repr.plot.width = 20, repr.plot.height =5)
  status_plot <- md2 %>% group_by(EventDate,Status) %>% tally() %>% ggplot(aes(x = EventDate, y = n,color=Status)) + geom_line() + geom_point(size =0.4) +scale_color_manual(values = c("Conflict" = "magenta","Cooperation"="lightskyblue","Neutral"="slategray4")) + ggtitle(paste0("Events between Source Country ",input$countrynamsource," and Target Country ",input$countrynamtarget," ",min(md2$Year),"-",max(md2$Year)))
  year_plotting <- ggplotly(status_plot) 
})


```


CAMEO Codes
=====================================  
  
  ### Events by CAMEO Code
  Neutral root codes: 01, 02 \
Cooperation root codes: 03, 04, 05, 06, 07, 08 \
Conflict root codes: 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20


```{r}
mydata1 <- reactive({
  conam1 <- input$countrynamsource
  conam2 <- input$countrynamtarget
  yr <- input$year
  mon <- input$month
  Rd_data1 <- subset(event_data, SourceCountry %in% conam1)
  Rd_data2 <- subset(Rd_data1, TargetCountry %in% conam2)
  Rd_data3 <- subset(Rd_data2, Year %in% yr)
  Rd_data4 <- subset(Rd_data3, MonthName %in% mon)
})


renderPlotly({
  shiny::validate(
    need(nrow(mydata1()) > 0, "No data for this selection.")
  )
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }
  md1 <- mydata1()
  bar_plotting<-ggplot(md1, aes(Root, fill = CAMEO))+ geom_bar()+scale_fill_manual(values = mycolors)+ xlab("Root Code") + ylab("Frequency") +  scale_y_continuous(breaks=integer_breaks())+ labs(fill="CAMEO Code")+ theme(axis.text.x = element_text(angle = 360, hjust = 1, face="bold",size=8),legend.direction = "horizontal", legend.position = "none") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ #add this bit to turn x-axis labels
    ggtitle(paste0("Events between Source Country ",input$countrynamsource," and Target Country ", input$countrynamtarget, " in ", input$month," ",input$year))
  bar_plotting
})

```

igraph and Map
=====================================  
  
  ```{r, include=FALSE}


library(dplyr)
library(lubridate)
library(igraph)
library(pacman)
library(magick)
library(haven)

##########2020##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2020'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
#V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2020.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2020",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2019##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2019'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
#V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2019.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2019",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2018##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2018'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
V(g)["Thailand"]$color <- "gold"
V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2018.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2018",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2017##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2017'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
#V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2017.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2017",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2016##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2016'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2016.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2016",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2015##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2015'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
#V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2015.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2015",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2014##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2014'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
#V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
#V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
#V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2014.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2014",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2013##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2013'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
#V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
#V(g)["Indonesia"]$color <- "gold"
#V(g)["Laos"]$color <- "gold"
V(g)["Malaysia"]$color <- "gold"
#V(g)["Myanmar"]$color <- "gold"
V(g)["Philippines"]$color <- "gold"
#V(g)["Singapore"]$color <- "gold"
#V(g)["Thailand"]$color <- "gold"
V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2013.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2013",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##########2012##########

Eventdata <- read_dta("ASEAN_Filter.dta") %>% as.data.frame
#View(Eventdata)

#Create dataframe with just necessary columns
df <- data.frame(Eventdata$Source_country, Eventdata$Target_country, Eventdata$CAMEO_Code, Eventdata$Event_Date)

#Create separate columns for year, month, date
df2 <- df %>%
  dplyr::mutate(year = lubridate::year(Eventdata.Event_Date), 
                month = lubridate::month(Eventdata.Event_Date), 
                day = lubridate::day(Eventdata.Event_Date))

#Convert CAMEO Code to root_code
library(stringr)
root_code <- str_extract(df2$Eventdata.CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
allData <- cbind(df2, root_code)
names(allData)[names(allData) == 'V1'] <- 'root_code'

#Filter by desired year
allData <- allData |> filter(allData$year %in% c('2012'))
#View(allData)

#Create matrix
matrix <- as.matrix(allData)

# Create graph object
g <- graph_from_data_frame(matrix)

# Color edges
E(g)$color <- as.factor(E(g)$root_code)
# Color edges lightskyblue for all cooperative root codes
E(g)[root_code %in% c("03", "04", "05", "06", "07", "08")]$color = "lightskyblue"
# Color edges magenta for all conflict root codes
E(g)[root_code %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]$color = "magenta"
# Color edges slategray4 for all neutral root codes
E(g)[root_code %in% c("01", "02")]$color = "slategray4"

# Color nodes
V(g)["Brunei"]$color <- "gold"
V(g)["Cambodia"]$color <- "gold"
V(g)["Indonesia"]$color <- "gold"
V(g)["Laos"]$color <- "gold"
V(g)["Malaysia"]$color <- "gold"
V(g)["Myanmar"]$color <- "gold"
V(g)["Philippines"]$color <- "gold"
V(g)["Singapore"]$color <- "gold"
V(g)["Thailand"]$color <- "gold"
V(g)["Vietnam"]$color <- "gold"
V(g)["China"]$color <- "red"

# Generate/save plot
png("ASEAN_2012.png", width = 890, height = 750)
plot (g, 
      main = "Cooperation, Conflict, and Neutral Actions Among China and ASEAN Countries: 2012",
      edge.arrow.size = 1,
      edge.arrow.width = .6, 
      vertex.label.cex = 1.2,
      vertex.size=20,
      layout=layout_in_circle)
legend("bottomleft", cex = 1.2, legend=c("China", "ASEAN", "Cooperation", "Conflict", "Neutral"),fill=c("red", "yellow", "lightskyblue", "magenta", "slategray4"))
dev.off()

##Animate igraphs Using Saved Images

#Read file names
images <- lapply(c("ASEAN_2012.png",
                   "ASEAN_2013.png",
                   "ASEAN_2014.png",
                   "ASEAN_2015.png",
                   "ASEAN_2016.png",
                   "ASEAN_2017.png",
                   "ASEAN_2018.png",
                   "ASEAN_2019.png",
                   "ASEAN_2020.png")
                 , image_read)
#Join images
images_joined <- image_join(images)

#Animate at one frame per second
images_gif <- image_animate(images_joined, fps = 0.5)

#View animated GIF
images_gif

#Save GIF
image_write(image = images_gif, path = "CHN_ASEAN_Animated.gif")
```

Column
-------------------------------------
  
  ### igraph of Interactions over Time
  ```{r}
images_gif
```


```{r map, include=FALSE, warning=FALSE, message=FALSE, error=FALSE}
library(ggplot2)
library(dplyr)
library(mapdata)
library(RColorBrewer)
asia <- ggplot2::map_data('world2',c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam", "China"))
class(asia)

asia <- mutate(asia, fill = case_when(
  region %in% c("China") ~ "red", 
  region %in% c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam") ~ "gold", 
  TRUE ~ "white"))

region.lab.data <- asia %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

map <- ggplot(asia, aes(x = long, y = lat)) +
  theme_void()+
  geom_map(map = asia, aes(map_id = region), fill="white", color="grey")+
  geom_polygon(aes(group = group, fill = fill), colour="grey60")+
  geom_text(aes(label = region), data = region.lab.data,  size = 3.1, hjust = 0.5)+
  scale_fill_identity()+
  ggtitle("Map of China and ASEAN Countires")+
  theme(plot.title = element_text(hjust = 0.5))
```

Column
-------------------------------------
  
  ### Map of China and ASEN Countries
  ```{r}
map
```



All igraphs {data-orientation=rows}
===================================== 
  
  ```{r}

knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2012.png?raw=true")
```


Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2013.png?raw=true")

```


Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2014.png?raw=true")
```

Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2015.png?raw=true")

```


Row
-------------------------------------
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2016.png?raw=true")

```

Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2017.png?raw=true")

```

Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2018.png?raw=true")

```


Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2019.png?raw=true")

```

Row
-------------------------------------
  
  ```{r}
knitr::include_graphics("https://github.com/kiranjaura/DataViz/blob/main/ASEAN_2020.png?raw=true")

```

Analysis {data-orientation=rows}
=====================================  
  
  ```{r, include=FALSE}
##Libraries
library(readxl)       # Read in Excel files
library(dplyr)        # Review data
library(tidyverse)    # Manipulate data
library(stats)        # Regressions functions
library(ISLR2)        # Regression functions
library(leaps)        # Regression functions
library(glmnet)       # Regression functions
library(pls)          # Regression functions
library(jsonlite)     # JSON parser and generator
library(jsonify)      # Converts Between 'R' Objects and Javascript Object Notation (JSON)
library(lubridate)    # Dates and times manipulate package
library(igraph)       # Package for algorithmic static graphical images
library(pacman)       # Allows loading packages (viz., library or require) & used to animate images
library(magick)       # Advanced Image-Processing package & used to animate images
library(stringr)      # Cohesive functions designed to work with strings
library(writexl)      # Allows exporting of df to Excel file
library(statnet)      # Exponential Random Graph Modeling (ERGM) package
library(network)      # Creates network objects from dataframes, etc. object
library(performance)  # Utilities for computing measures to assess model quality
library(easystats)    # Provides consistent framework for R statistics analysis
library(report)       # easystats data frame report
library(ggplot2)      # Graphical modules
library(plotly)       # Graphical modules
library(scales)       # Calculation tool


##Variables
#Set working directory path, data sub path, and file names


File_json <-  "ASEAN_Eventdata.json"
File_excel01 <- "EPPS6356_Data4R_TEMP.xlsx"
#File_excel02 <- "EPPS6356_DatafromR.xlsx"
File_excel03 <- "China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx"

##Load JSON file and create data frame
Eventdata <-
  fromJSON("https://raw.githubusercontent.com/kiranjaura/DataViz/main/ASEAN_Eventdata.json") %>% as.data.frame

#Create dataframe using "Source Country," "Target Country," "CAMEO Code," and "Event Date" columns
#using transitory data files: df1, df2, etc. and root_code
df1 <-
  data.frame(
    Eventdata$`Source Country`,
    Eventdata$`Target Country`,
    Eventdata$`CAMEO Code`,
    Eventdata$`Event Date`
  )

#Change column names
colnames(df1) <-
  c("Source_Country",
    "Target_Country",
    "CAMEO_Code",
    "Event_Date") 

#Create separate columns for year (i.e, "year = lubridate::year(Event_Date)")
#leaving out month (i.e., "month = lubridate::month(Event_Date)") 
#and date (i.e., "day = lubridate::day(Event_Date)")
df2<- df1 %>%
  dplyr::mutate(year = lubridate::year(Event_Date))

#Convert CAMEO Code to root_code
#Create matrix
matrix <- as.matrix(df2)
#Convert codes, create new dataframe and remain column "V1" to root_code
#*Neutral sentiment codes: 01, 02
#*Cooperation sentiment codes: 03, 04, 05, 06, 07, 08
#*Conflict sentiment codes: 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
root_code<-str_extract(df2$CAMEO_Code, "^\\d{2}")
root_code <- as.data.frame(matrix(root_code, ncol = 1, byrow = TRUE))
df3 <- cbind(df2, root_code)
names(df3)[names(df3) == 'V1'] <- 'root_code'

#Filter to get the desired Source Countries (that match country demographic)
#  data and not Target Countries (excluding China from Target Countries)
#  not including "CAMEO_Code", "Event_Date" values
#  and including only selected years
df4 <- data.frame(df3$Source_Country, df3$Target_Country, df3$year, df3$root_code) |>
  #Filter on China as Source_Country and ASEAN Countries as Target_Country
  filter(df3.Source_Country %in% c("China")) |>
  filter(df3.Target_Country %in% c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore",
                                   "Thailand", "Vietnam"))|>
  #Filter by desired year 
  filter(df3.year %in% c('2012','2013','2014','2015','2016','2017','2018','2019','2020'))
#UNUSED FILTERS:
#Filter by desired month (January = '1'; February = '2'; March = '3'; April = '4')
#filter(df3.month %in% c('1'))

#Remove China as a Target_Country because China cannot be "eventing" with itself
df4<- subset(df4, df3.Target_Country != "China")


#Change column names to be compatible with PREDICTORS_df (see below)
colnames(df4) <-
  c("COUNTRY", "Target_Country",
    "YEAR",
    "CAMEO_root") 

#Move the YEAR column after the COUNTRY column to be consistent with PREDICTORS_df (see below)
df4 <- df4 %>% relocate(YEAR, .after = COUNTRY)

#Remove all rows with NAs
df5 <- na.omit(df4)

#Transform dataframe CAMEO_root into numeric rather than characters
df5 <- transform(df5, CAMEO_root = as.numeric(CAMEO_root))

#Add the category of CAMEO code to df as CAMEO_Status
df5 <-
  df5 %>% plyr::mutate(
    CAMEO_Status = dplyr::case_when(
      CAMEO_root %in% c(1, 2) ~ "Neutral",
      CAMEO_root %in% c(3, 4, 5, 6, 7, 8) ~ "Cooperative",
      CAMEO_root %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ~ "Conflictual"
    )
  ) 

#Add the category of CAMEO code to df as CAMEO_Status_val
df5 <-
  df5 %>% plyr::mutate(
    CAMEO_Status_val = dplyr::case_when(
      CAMEO_root %in% c(1, 2) ~ 0,
      CAMEO_root %in% c(3, 4, 5, 6, 7, 8) ~ 1,
      CAMEO_root %in% c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) ~ -1
    )
  ) 


#Create final dataframe by removing all originating countries except China
#and removing Target_Country where China is the target (no "China" on "China" communication)
df6 <- df5 %>% as.data.frame

China_df_ASEAN_CAMEO_Targets<- df6

##Clean up this section
#Remove transitory data files and variables
rm(df1, df2, df3, df4, df5, df6, Eventdata, matrix, root_code)


##############################################################################
#                Load Data from National Demographics File                   #
##############################################################################

##Load Excel file and create data frame
#Read the Excel R&D response & predictors file and load into dataframe
df1 <-
  read_excel("EPPS6356_Data4R_TEMP.xlsx",
             col_names = TRUE,
             na = "",
             trim_ws = TRUE
  ) 

#Convert data to data frame and create final dataframe
PREDICTORS_df <-
  as.data.frame(df1)


##Merge dataframes
#Merge preliminary dataframes based on Country and Year
df2 <- merge(China_df_ASEAN_CAMEO_Targets, PREDICTORS_df, by.ASEAN_df=c("COUNTRY", "YEAR"))

#Transform dataframe into numeric rather than characters
#*Variable descriptions:
# COUNTRY = Country Name
# YEAR = Year
# CAMEO_root_mean = Average CAMEO code from Country by year
# DEMINDX = Economist Intelligence Unit's Democracy Index
# EFCPI = Commercial and professional infrastructure
# EFIMD = Internal market dynamics
# EFIMO = Internal market openness
# INDPERINT = Percentage of Individuals using the Internet 
# ACCESSELEC = Access to electricity (% of population)
# ARMFORCEPERLABOR = Armed forces personnel (% of total labor force)
# GDPMKTP = GDP growth (annual %)
# GDITOTL = Gross capital formation (% of GDP)
# MILXPND = Military expenditure (% of GDP)
# TRDPERGDP = Trade (% of GDP)
# FMBACH = Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)
# MABACH = Educational attainment, at least Bachelor's or equivalent, population 25+, male (%) (cumulative)
# GINI = GINI index (World Bank estimate)
# FMPAR = Proportion of seats held by women in national parliaments (%)
# FMIN = Proportion of women in ministerial level positions (%)
# STRGLEG = Strength of legal rights index (0=weak to 12=strong)
df2 <- transform(df2, 
                 CAMEO_root = as.numeric(CAMEO_root),
                 CAMEO_Status_val= as.numeric(CAMEO_Status_val),
                 DEMINDX = as.numeric(DEMINDX),
                 EFCPI = as.numeric(EFCPI),
                 EFIMD = as.numeric(EFIMD),
                 EFIMO = as.numeric(EFIMO),
                 INDPERINT = as.numeric(INDPERINT),
                 ACCESSELEC = as.numeric(ACCESSELEC),
                 ARMFORCEPERLABOR = as.numeric(ARMFORCEPERLABOR),
                 GDPMKTP = as.numeric(GDPMKTP),
                 GDITOTL = as.numeric(GDITOTL),
                 MILXPND = as.numeric(MILXPND),
                 TRDPERGDP = as.numeric(TRDPERGDP),
                 FMBACH = as.numeric(FMBACH),
                 MABACH = as.numeric(MABACH),
                 GINI = as.numeric(GINI),
                 FMPAR = as.numeric(FMPAR),
                 FMIN = as.numeric(FMIN),
                 STRGLEG = as.numeric(STRGLEG))


#Move the CAMEO_Status column after the CAMEO_root column
df2 <- df2 %>% relocate(CAMEO_Status, .after = CAMEO_root)

##Final Base dataframe creation
Base_df <- as.data.frame(df2)

#Group by Country and Year and calculate mean values for all variables;
#Note: for all variables that are not CAMEO_root the mean will just be
#the yearly value because that value did not vary during the year
Base_df_mean <- Base_df %>% 
  group_by(COUNTRY, Target_Country, YEAR) %>% 
  summarise(across(c(CAMEO_root, CAMEO_Status_val, DEMINDX, EFCPI, EFIMD, EFIMO, 
                     INDPERINT, ACCESSELEC,ARMFORCEPERLABOR, 
                     GDPMKTP, GDITOTL, MILXPND, TRDPERGDP, 
                     FMBACH, MABACH, GINI, FMPAR, 
                     FMIN, STRGLEG), mean))

#Change name of CAMEO_root and CAMEO_Status_val
names(Base_df_mean)[names(Base_df_mean) == 'CAMEO_root'] <- 'CAMEO_root_mean'
names(Base_df_mean)[names(Base_df_mean) == 'CAMEO_Status_val'] <- 'CAMEO_Status_val_mean'

##Clean up this section
#Write dataframe to Excel file for future review
#write_xlsx(Base_df_mean, paste(Path_wd, Path_data, File_excel02 ,sep = ""))
#Remove transitory data files and variables

rm(df1, df2, PREDICTORS_df)

##############################################################################
#               Load Data from China Investment Tracker                      #
##############################################################################

#Process data from https://www.aei.org/china-global-investment-tracker/ 
#in China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx

df1 <-
  read_excel("China-Global-Investment-Tracker-2022-SPRING-final-1.xlsx",
             col_names = TRUE,
             na = "",
             trim_ws = TRUE
  ) 


##Convert data to data frame and create final dataframe
df2 <-
  as.data.frame(df1)
#Rename column(s)
df2 <- df2 %>% rename("Invest_by_China_mils" = "Quantity in Millions")

#Extract data
df3 <- data.frame(df2$Country, df2$Year, df2$Invest_by_China_mils) |>  
  # Filter by ASEAN countries
  filter(df2.Country %in% c("Cambodia", "Indonesia", "Laos", "Malaysia", 
                            "Myanmar", "Philippines", "Singapore", "Thailand",
                            "Vietnam", "Brunei")) |>
  filter(df2.Year %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

#Convert data to data frame and create final dataframe
df4 <-
  as.data.frame(df3)

colnames(df4) <-
  c("Target_Country",
    "YEAR",
    "Invest_by_China_mils") 

#Add the column for COUNTRY and fill it with China
df4 <- add_column(df4, COUNTRY = "China", .before = "Target_Country")

#Transform dataframe into numeric rather than characters
#*Variable descriptions:
#Year = Year 
#Country = Country Name
#Invest_by_China_mils =  Investment made by China, in millions
df4 <- transform(df4, 
                 Invest_by_China_mils = as.numeric(Invest_by_China_mils))

#Calculate the sum of China investment by Target_Country and YEAR
#and create a dataframe China_ASEAN_Invest
China_df_ASEAN_Invest <- df4 %>% 
  group_by(COUNTRY, Target_Country, YEAR) %>% 
  summarise(across(Invest_by_China_mils, sum))

#Move the YEAR column after the COUNTRY column to be consistent with other dataframes
China_df_ASEAN_Invest <- China_df_ASEAN_Invest %>% relocate(YEAR, .after = COUNTRY)

#Merge the China_ASEAN_Invest dataframe into the Base_df_mean dataframe 
#keeping all Base_df_mean values using a left join strategy
Base_df_meanCAMEO_invest <- merge(x=Base_df_mean, y=China_df_ASEAN_Invest,
                                  by=c("COUNTRY", "Target_Country", "YEAR"), all.x=TRUE)

#Move the YEAR column after the COUNTRY column to be consistent with other dataframes
Base_df_meanCAMEO_invest <- Base_df_meanCAMEO_invest %>% relocate(YEAR, .after = COUNTRY)


#Remove Base_df_mean as now redundant #with the data in Base_df_mean_invest
rm(Base_df_mean)

##Clean up this section
#Remove transitory data files and variables
rm(df1, df2, df3, df4)


#*Turn off warings
options(warn=-1)

#*Construct graphic
Plot3D00 <- plot_ly(x=Base_df_meanCAMEO_invest$Target_Country, y=Base_df_meanCAMEO_invest$YEAR, 
                    z=Base_df_meanCAMEO_invest$CAMEO_Status_val_mean, type="scatter3d",
                    mode="markers")
Plot3D01 <- Plot3D00  %>%  add_markers()
Plot3D01 <- Plot3D01 %>% layout(scene = list(xaxis = list(title = 'Target Country'),
                                             yaxis = list(title = 'Year'),
                                             zaxis = list(title = 'CAMEO: Coop. = 1, Neutral = 0, Conflict= -1')))
Plot3D01 <- Plot3D01 %>% layout(title = 'Annual Mean CAMEO by Year & Target Country',
                                plot_bgcolor = "#e5ecf6")
#Set up layout formatting
axx <- list(
  ticks="outside",
  tickangle=45, 
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(200, 200, 230",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)

axy <- list(
  ticks="outside",
  tickangle=45, 
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(230, 200,230)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
axz <- list(
  tickfont = list(family='Courier', color='orange', size=12),
  backgroundcolor="rgb(230, 230,200)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255"
)
#Layout Display
Plot3D01 <- Plot3D01 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
Plot3D01 <- Plot3D01 %>% layout(showlegend = FALSE)
Scatter_3D_by_country_year <- Plot3D01

##Clean up this section / Remove transitory data files and variables
rm(Plot3D00, Plot3D01, axx, axy, axz)


#*Turn on warings
options(warn=0)
```



```{r}
#Display Graph
Scatter_3D_by_country_year
```

Row
-------------------------------------
  
  ```{r}
##############################################################################
#                              Bar Charts                                    #
##############################################################################

##Bar Chart 01
#*Title:        Bar plot by Country on CAMEO_Status
#*Propose:      Determine which ASEAN countries are being impacted by events between
#*               China and ASEAN countries.
#*#*Conclusion: Most CAMEO Codes are between China and Cambodia, resulting in
#*              little information available to show variance between ASEAN countries.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Bar00 <- ggplot(Base_df, aes(x=Target_Country, fill=CAMEO_Status))
Bar01 <- Bar00 + theme_bw() + geom_bar()
Bar01 <- Bar01 + labs(x="Target ASEAN Country", y="Count CAMEO Type", 
                      title = "CAMEO Status by Country")
Bar01 <- Bar01 +  guides(fill=guide_legend(title = "CAMEO Status"))
Bar_CAMEO_by_country <- Bar01

##Clean up this section / Remove transitory data files and variables
rm(Bar00, Bar01)

#Display Graph
Bar_CAMEO_by_country
```

Row
-------------------------------------
  
  ```{r, include=FALSE}
#*Turn on warings
options(warn=0)


##############################################################################
#                              Scatter Charts                                #
##############################################################################

##* Scatter Chart 01
#*Title:      Scatter Plot of Value of CAMEO Status vs China Investment 
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            annual investments in that country and to project useful correlations.
#*Conclusion: There is little correlation between CAMEO status and China's investment

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=Invest_by_China_mils/1000,
                                                  shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to China Investment")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value (Cooperation=1, Neutral=0, Conflict=-1)", 
                              y="Annual Invest by China (Bil)")
Scatter01 <- Scatter01 + xlim(0.8, 1.0) #Limit x axis values to better display graphic
Scatter01 <- Scatter01 +ylim(0.0, 7.0) #Limit y axis values to better display graphic
Scatter_China_Invest <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)
```

Row
-------------------------------------
  
  ```{r}

#Display Graph
Scatter_China_Invest

#*Turn on warings
options(warn=0)


##* Scatter Chart 02
#*Title:      Scatter Plot of Value of CAMEO Status vs Democracy Index
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            Democracy Index values and to project useful correlations.
#*Conclusion: While there is possibly some trend the graph is showing two separate
#*            trend lines and has a very wide confidence interval.  No correlation indicated.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=DEMINDX, shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to Democracy Index")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value", y="Democracy Index")
Scatter01 <- Scatter01 + xlim(0.7, 1.00) #Limit x axis values to better display graphic
Scatter_Dem_Indx <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)
```

Row
-------------------------------------
  
  ```{r}
#Display Graph
Scatter_Dem_Indx

#*Turn on warings
options(warn=0)


##* Scatter Chart 03
#*Title:      Scatter Plot of Value of CAMEO Status vs Military Expenditure as Percent of GDP 
#*Purpose:    To compare CAMEO mean values based on the average CAMEO status values
#*            (i.e., 1 = Cooperation, 0 = Neutral, and -1 = Conflict) to country 
#*            annual military expenditure as a percent of GDP and to project useful correlations.
#*Conclusion: There is little correlation between CAMEO status and the amount of
#*            military expenditures by ASEAN countries.

#*Turn off warings
options(warn=-1)

#*Construct graphic
Scatter00 <- ggplot(Base_df_meanCAMEO_invest, aes(x=CAMEO_Status_val_mean, 
                                                  y=MILXPND,
                                                  shape=Target_Country,
                                                  color=Target_Country))
Scatter01 <- Scatter00 + theme_classic()
Scatter01 <- Scatter01 + geom_point(size=2, shape=24)
Scatter01 <- Scatter01 + geom_rug()
Scatter01 <- Scatter01 + geom_smooth(method=lm, linetype="dashed", 
                                     color="red", fill="lightblue")
Scatter01 <- Scatter01 + ggtitle("CAMEO Mean Status Value to Military Expense % GDP")
Scatter01 <- Scatter01 + labs(colour = "Targeted Country")
Scatter01 <- Scatter01 + labs(x="CAMEO Mean Status Value (Cooperation=1, Neutral=0, Conflict=-1)", 
                              y="Military Expenditures percent of GDP")
Scatter_Military_Exp <- Scatter01

##Clean up this section / Remove transitory data files and variables
rm(Scatter00, Scatter01)
```

Row
-------------------------------------
  
  ```{r}
#Display Graph
Scatter_Military_Exp

#*Turn on warings
options(warn=0)



```