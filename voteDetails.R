# Set the working directory to where you put the data files.

# Read in the file extracted from the detail.txt file downloaded from
# https://results.enr.clarityelections.com/GA/91639/Web02-state.216038/#/
dat <- read.table('detailGov.txt', header=T, skip=1, stringsAsFactors=F)

# Extract a data frame that counts the proportion of paper (absentee) ballot
# votes and the proportion of machine ballot votes that were for Kemp.
adR <- data.frame(County = dat$County, absentee = dat$R.Absentee.by.Mail / 
	(dat$R.Absentee.by.Mail + dat$D.Absentee.by.Mail + dat$L.Absentee.by.Mail), 
	machine = (dat$R.Advance.in.Person + dat$R.Election.Day) /
	(dat$R.Advance.in.Person + dat$D.Advance.in.Person + dat$L.Advance.in.Person + 
	dat$R.Election.Day + dat$D.Election.Day + dat$L.Election.Day))
# Remove the row for the "Totals:" from this data frame.
adR <- adR[1:159, ]

# Read in the file of numbers that were finally posted on 2018-11-09 at
# http://sos.ga.gov/index.php/elections/november_6_2018_general_election_unofficial_provisional_count
dat2 <- read.table('provisional.txt', header=T, stringsAsFactors=F)

# Compare the reported numbers to those reported at clarityelections.com
sum(dat[160, grep("Provisional", names(dat))]) # [1] 35
dat2[160, 2] # [1] 21358

# Make a new plot showing the relationship between the two proportions in adR,
# with circle sized by number of outstanding provisional ballots.
rownames(adR) <- adR$County
rownames(dat2) <- dat2$County
adR$provisional <- dat2$Number[1:159]
adR$cex <- (adR$provisional + 1) / 1000
quartz(width=4, height=4)
par(oma=rep(0,4), mar=c(3,3,0.1,0.1), mgp=c(1.7,0.4,0))
plot(machine ~ absentee, adR, xlim=c(0,1), ylim=c(0,1), type="n", xaxs="i", yaxs="i",
	xlab = "Kemp proportion absentee (paper) ballots", 
	ylab = "Kemp proportion votes by machine")
abline(a=0, b=1, lty=3)
points(machine ~ absentee, adR, pch=20, cex=adR$cex)
mtext("points sized by # provisional ballots ", side = 1, line=-1, adj=1)

# Number of counties where Kemp would have won by either count. 
length(which(adR$machine > 0.5 & adR$absentee > 0.5)) # [1] 74
# Number of counties where Kemp would have won only by the DRE votes count. 
length(which(adR$machine > 0.5 & adR$absentee < 0.5)) # [1] 58
# Number of counties where Kemp would not have won by either count. 
length(which(adR$machine < 0.5 & adR$absentee < 0.5)) # [1] 27

# Here is the number of votes overall by which Kemp purportedly exceeded 50%.
dat$R.Choice.Total[160] - (dat$Total[160] / 2)
# [1] 12811


# Make a second plot showing the relationship between Kemp's advantage against
# the numer of provisional ballots. 
adR$KempLead <- dat$R.Choice.Total[1:159] - dat$D.Choice.Total[1:159]
quartz(width=4, height=4)
par(oma=rep(0,4), mar=c(3,3,0.1,0.1), mgp=c(1.7,0.4,0))
plot(provisional ~ KempLead, adR, pch=20, 
	ylab = "Number of:   ... outstanding provisional ballots", 
	xlab = "... votes for Kemp minus votes for Abrams")
