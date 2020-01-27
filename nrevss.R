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

# =============================================================================
# Examine lab-confirmed flu data
# =============================================================================

labflu <- read.csv('LabFluByWeek.csv', colClasses=c("integer","integer","character","integer","integer","integer","integer","integer","integer","integer","integer","integer"))
labflu <- as_tibble(labflu)
yearweekconversion <- read.csv('YearWeekConversion.csv', colClasses=c("integer","integer","character"))
yearweekconversion$Date <- as_date(yearweekconversion$Date)
yearweekconversion <- as_tibble(yearweekconversion)


totallabfludf <- labflu %>% 
	mutate(totals = A..H1. + A..Unable.to.Subtype. + A..H3. + A..H1N1.pdm09 + A..Subtyping.not.Performed. + B..Victoria.Lineage. + B..Yamagata.Lineage. + B..Lineage.Unspecified. + H3N2v
		) %>%
	select(Year, Week, Age.Group, totals) %>% 
	group_by(Year, Week) %>% 
	summarise(totals = sum(totals)) %>%
	left_join(yearweekconversion, by=c("Year","Week")) %>%
	ungroup() %>% 
	select(Date, totals)


fignatcovlabflu <- natcov %>% 
	left_join(totallabfludf, by=c("Week"="Date")) %>% 
	rename(Flu="totals") %>% 
	mutate(Flu = Flu/500) %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","Flu"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","black","magenta","green")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() 	


# =============================================================================
# Examine ILINet data
# =============================================================================

ili <- read.csv('ILINet.csv', colClasses=c("character","NULL","integer","integer","numeric","numeric","integer","integer","NULL","integer","integer","integer","integer","integer","integer"))
ili <- as_tibble(ili)

ilidf <- ili %>% 
	select(YEAR, WEEK, PCTWEIGHTEDILI) %>% 
	left_join(yearweekconversion, by=c("YEAR"="Year","WEEK"="Week")) %>%
	ungroup() %>% 
	select(Date, PCTWEIGHTEDILI) %>% 
	rename("WEEK"="Date")

fignatcovili <- natcov %>% 
	left_join(ilidf, by=c("Week"="WEEK")) %>% 
	rename(ILI="PCTWEIGHTEDILI") %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","ILI"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","black","magenta","green")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive (CoV) or Percent Weighted ILI")

# =============================================================================
# A better lab-confirmed flu analysis: NREVSS clinical flu
# =============================================================================

nrevssflu <- read.csv('NREVSSClinicalFlu.csv', colClasses=c("NULL","NULL","integer","integer","integer","integer","integer","numeric","numeric","numeric"))
nrevssflu <- as_tibble(nrevssflu)

nrevssfludf <- nrevssflu %>% 
	select(YEAR, WEEK, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	left_join(yearweekconversion, by=c("YEAR"="Year","WEEK"="Week")) %>%
	ungroup() %>% 
	select(Date, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	rename("WEEK"="Date")

fignatcovnrevssflu <- natcov %>% 
	left_join(nrevssfludf, by=c("Week"="WEEK")) %>% 
	rename(Flu="PERCENTPOSITIVE") %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","Flu"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","black","magenta","green")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive")


fignatcovnrevssflu_spec <- natcov %>% 
	left_join(nrevssfludf, by=c("Week"="WEEK")) %>% 
	rename(Flu="TOTALSPECIMENS") %>% 
	mutate(Flu=Flu/10000) %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","Flu"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","black","magenta","green")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive")














