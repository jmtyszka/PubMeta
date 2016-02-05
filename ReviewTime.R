# Collect and analysis publication review history from PubMed
#
# AUTHOR : Mike Tyszka, Ph.D.
# PLACE  : Caltech
# DATES  : 2016-01-27 JMT From Dave Tang's HowTo
#
# REFERENCES : http://davetang.org/muse/2013/10/31/querying-pubmed-using-r/

# Use RISmed library
library(RISmed)
library(ggplot2)

# Journal to analyse
journal.name="Neuron"

# Flags
do.usefile = TRUE

# Change working directory
setwd("/Users/jmt/GitHub/PubMeta")

# PubMed search including start date
pubmed.search = paste(journal.name,"[ta]",sep="")

# File names
journal.stub=gsub(" ","",journal.name)
meta.file = paste(journal.stub,".rda",sep="")
histo.file = paste(journal.stub,"_histogram.pdf",sep="")
trend.file = paste(journal.stub,"_trend.pdf",sep="")

# Check for existing data
if (file.exists(meta.file) && do.usefile) {
  
  # Load existing data frame
  cat("Loading existing metadata from file\n")
  load(meta.file)
  
} else {
  
  cat("Running new search\n")
  
  # Perform PubMed search
  cat("  Querying PubMed\n")
  query <- EUtilsSummary(pubmed.search, type='esearch', db='pubmed',retmax=2500)
  
  # Extract the metadata for matched records
  cat("  Fetching metadata\n")
  fetch <- EUtilsGet(query)
  
  # Save metadata
  cat("  Saving metadata")
  save(fetch, file=meta.file)
  
}

# PMIDs
ID <- PMID(fetch)

# Received date
year.rec <- YearReceived(fetch)
month.rec <- MonthReceived(fetch)
day.rec <- DayReceived(fetch)
date.rec <- ISOdate(year.rec,month.rec,day.rec)

# Accepted date
year.acc <- YearAccepted(fetch)
month.acc <- MonthAccepted(fetch)
day.acc <- DayAccepted(fetch)
date.acc <- ISOdate(year.acc,month.acc,day.acc)

# Review time in days
review.time <- as.numeric(difftime(date.acc, date.rec), units="days")

# Remove unrealistic values and NAs
cat("Removing negative and NA review times")
realistic <- !is.na(review.time) & review.time >= 0.0
date.acc <- date.acc[realistic]
review.time <- review.time[realistic]
ID <- ID[realistic]

# Report any review times over 18 months (540 days)
cat("Unusually long review times\n")
long.time <- review.time > 540
long <- data.frame(ID[long.time], review.time[long.time])
print(long)

# Create data frame for review times, received dates
review.data <- data.frame(date.acc,review.time)

# Plot histogram of review time

plot.title = paste(journal.name,"Review Times for Last 2500 articles")
  
# Histogram of review times
vp <- ggplot(review.data, aes(x = review.time))
vp <- vp + geom_histogram(bins=50, origin=0.0)
vp <- vp + xlab("\nReview Time (days)") + ylab("Count")
vp <- vp + ggtitle(plot.title)
print(vp)
ggsave(filename=histo.file, plot=vp)

# Review times trend
vp <- ggplot(review.data, aes(x=date.acc,y=review.time))
vp <- vp + geom_point(color='lightblue')
vp <- vp + stat_smooth(color='black', se=TRUE, level=0.95)
vp <- vp + xlab("Date Accepted") + ylab("Review Time (days)\n")
vp <- vp + ggtitle(plot.title)
print(vp)
ggsave(filename=trend.file, plot=vp)