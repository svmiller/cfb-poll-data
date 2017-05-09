setwd("~/Dropbox/data/cfb-poll-data")

library(rvest)
library(stringr)
library(knitr)

# Coaches
# -------

coaches <- read_html("http://sportspolls.usatoday.com/ncaa/football/polls/coaches-poll/")

coachesweek <- coaches %>% 
  html_node("h4.secondary_title") %>%
  html_text()

coachesweek <- as.numeric(gsub(" TOP 25 TEAMS, WEEK ","",coachesweek))

Coaches = html_table(html_nodes(coaches, "table")[[1]])

names(Coaches) <- c("Rank","Team","Record","Points", "No. 1 Votes",
                 "PV Rank", "Change", "Hi/Low")

Coaches <- Coaches[2:nrow(Coaches),]
Coaches$Week <- coachesweek

write.table(Coaches,file=paste("2016/coaches/2016-coaches-poll-",coachesweek,".csv",sep=""),sep=",",row.names=F,na="") 


# Associated Press
# ----------------

apcfb <- read_html("http://collegefootball.ap.org/poll")

apweek <- apcfb %>% 
  html_node("#block-ap-poll-top-25-left-nav .block-title") %>%
  html_text()

Poll = html_table(html_nodes(apcfb, "table")[[1]])

Poll$X2 <- NULL

Poll <- cbind(Poll[,c("X1", "X4")], str_split_fixed(Poll$X3, "Record: ", 2))

names(Poll) <- c("rank","fixme","team", "record")

confs <- c("SEC","ACC","Big 12", "Big Ten",
           "Division I FBS Independents", 
           "The American", "Pac-12", "Mountain West",
           "Mid-American", "Sun Belt")

conflist <- str_c(confs, collapse="|")
Poll$conference <- unlist(str_extract_all(Poll$team, conflist))
Poll$team <- str_split_fixed(Poll$team, conflist, 2)[, 1]

Poll$conference[Poll$conference == "Division I FBS Independents"] <- "Independent"

Poll$fpv <- str_extract_all(Poll$team, 
                            " \\(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\)")
Poll$team <- str_split_fixed(Poll$team, 
                             " \\(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\)", 2)[, 1]

Poll$fpv <- as.numeric(gsub("\\(|\\)", "", Poll$fpv))

# Still I wish I knew what to do with this.
Poll$fixme <- gsub("PV Rank|Points", "", Poll$fixme)
Poll$fixme <- NULL

Poll$pv_rank <- NA

#cbs <- read_html("http://www.cbssports.com/collegefootball/rankings/ap")
#CBS = html_table(html_nodes(cbs, "table")[[1]])

#Poll$points <- CBS$PTS
Poll$points <- NA

Poll$week <- apweek

names(Poll) <- c("Rank","Team","Record","Conference", "No. 1 Votes",
                 "PV Rank", "Points", "Week")
Poll <- Poll[,c(8,1,2,5,3,4,6,7)]

apcaption <- paste("The AP College Football Poll (",apweek,")", sep="")

cat(gsub('\\bNA\\b', '  ', kable(Poll[,2:ncol(Poll)], format="html", 
                                 caption=apcaption, pad=0, align=c("c"))), sep='\n')

# Get individual-level voter data.
# --------------------------------

voters <- apcfb %>%
  html_nodes(".voter-menu") %>%
  html_text()

voters <- gsub("\t\t\t\t\t\t", ",", voters)
voters <- gsub("\n","", voters)
voters <- gsub("\n\t\t\t","", voters)
voters <- gsub(",\t\t\t","", voters)

voters <- unlist(strsplit(voters, ",  "))
voters <- voters[-1]

voterurl <- tolower(voters)
voterurl <- gsub(" ", "-", voterurl)

# Sorry, Mandy. Blame the AP's website.
#voterurl <- voterurl[voterurl != "mandy-mitchell"] 

# Marq Burnett didn't submit one.
voterurl <- voterurl[voterurl != "marq-burnett"] 


# Rank, Team, Record, Points, PV Rank, Voter

AP <- Poll[,c(2,3,5,8,7)]
AP$Voter <- "ap"



for (k in voterurl) {
  url <- paste("http://collegefootball.ap.org/poll-voter/", k, sep="")
  voterpoll <- read_html(url)
  poll = html_table(html_nodes(voterpoll, "table")[[1]])
  poll$Voter <- k
  AP <- rbind(AP, poll)

}

AP$Team <- gsub(" *\\(.*?\\) *", "", AP$Team)
AP$Points <- as.numeric(gsub(",", "", AP$Points))

d<-na.omit(AP)
AP <- transform(AP,"PV Rank"=d$"PV Rank"[match(Team,d$Team)])
AP <- transform(AP,Points=d$"Points"[match(Team,d$Team)])

cat(gsub('\\bNA\\b', '  ', kable(cbind(Poll[,2:5],AP[1:25,c(5,4)]), 
                                 format="html", 
                                 col.names=c("Rank", "Team", "#1 Votes", "Rec.", "Prev", "Points"),
                                 caption=apcaption, pad=0, align=c("c"))), sep='\n')

AP$Week <- apweek

apweek <- ifelse(apweek == "Pre-Season", 1, gsub("Week ","",apweek))
apweek <- ifelse(apweek == "Final", 16, gsub("Week ","",apweek))

library(reshape2)

VoterWide <- dcast(AP, Team + Record + Points + Week ~ Voter, value.var = 'Rank')
VoterWide <- VoterWide[order(-VoterWide$Points),]
VoterWide <- VoterWide[,c(1:4,8,5:7,9:ncol(VoterWide))]

write.table(VoterWide,file=paste("2016/ap/2016-ap-poll-voter-data-wide-",apweek,".csv",sep=""),sep=",",row.names=F,na="") 
write.table(AP,file=paste("2016/ap/2016-ap-poll-voter-data-long-",apweek,".csv",sep=""),sep=",",row.names=F,na="") 
write.table(cbind(Poll[,1:6],AP[1:25,c(5,4)]),file=paste("2016/ap/2016-ap-poll-",apweek,".csv",sep=""),sep=",",row.names=F,na="") 

