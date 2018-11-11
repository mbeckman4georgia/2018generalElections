#### First run the code in the voteDetails.R file.

# Read in the file extracted from the new detail.txt file downloaded from
# https://results.enr.clarityelections.com/GA/91639/Web02-state.216038/#/
# on 2018-11-11 after seeing the announcement at
# http://sos.ga.gov/index.php/general/secretary_crittenden_offers_clarifications_on_election_certification
dat3 <- read.table('detail2Gov.txt', header=T, stringsAsFactors=F)

# Compare the number of still outstanding provisional ballots after deadline:
sum(dat3[160, grep("Provisional", names(dat3))])
# [1] 4490
dat2[160, "Number"]
# [1] 21358

# Compare the new number of votes overall by which Kemp purportedly exceeded 50%.
dat3$R.Choice.Total[160] - dat3$Total[160] / 2
# [1] 10863