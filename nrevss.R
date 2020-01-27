# =============================================================================
# Import
# =============================================================================

library(tidyverse)
library(lubridate)

natcov <- read.csv('NationalCoV.csv', colClasses=c("character","numeric","numeric","numeric","numeric"))
natcov$Week <- as_date(natcov$Week)
natcov <- as_tibble(natcov)

# =============================================================================
# Plot % positive by virus over time
# =============================================================================

fignatcov <- natcov %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E"), names_to="Virus", values_to="PercentPositive") %>%
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","black","magenta")) + 
		# scale_color_brewer(type="qual", palette=4) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() 

ggsave(fignatcov, file="~/DropboxHarvard/Projects/Wuhan/natcov.pdf", width=7, height=5)

# =============================================================================
# Plot number of tests over time from Killerby et al.
# =============================================================================


ntests <- read.csv('ntests.csv', colClasses=c("integer","character"))
ntests$WEEK <- as_date(ntests$WEEK)
ntests <- as_tibble(ntests)

figtestsperweek <- ntests %>% 
	ggplot(aes(x=WEEK, y=NTESTS)) + 
		geom_col() + 
		expand_limits(y=c(0,14000))  + 
		scale_y_continuous(breaks=seq(from=0, to=14000, by=2000)) + 
		labs(x="", y="No. of tests/week") + 
		theme_minimal() + 
		theme(text=element_text(size=16))

ggsave(figtestsperweek, file="~/DropboxHarvard/Projects/Wuhan/testsperweek.pdf", width=12, height=5)