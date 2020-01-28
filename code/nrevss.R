# =============================================================================
# Import
# =============================================================================

library(tidyverse)
library(lubridate)

natcov <- read.csv('data/NationalCoV.csv', colClasses=c("character","numeric","numeric","numeric","numeric"))
natcov$Week <- as_date(natcov$Week)
natcov <- as_tibble(natcov)

yearweekconversion <- read.csv('data/YearWeekConversion.csv', colClasses=c("integer","integer","character"))
yearweekconversion$Date <- as_date(yearweekconversion$Date)
yearweekconversion <- as_tibble(yearweekconversion)


# =============================================================================
# Plot % positive by virus over time
# =============================================================================

fignatcov <- natcov %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E"), names_to="Virus", values_to="PercentPositive") %>%
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","green","magenta")) + 
		# scale_color_brewer(type="qual", palette=4) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() 

ggsave(fignatcov, file="figures/natcov.pdf", width=7, height=5)

# =============================================================================
# Plot number of tests over time from Killerby et al.
# =============================================================================

ntests <- read.csv('data/ntests.csv', colClasses=c("integer","character"))
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

ggsave(figtestsperweek, file="figures/testsperweek.pdf", width=12, height=5)

# =============================================================================
# Examine ILINet data
# =============================================================================

ili <- read.csv('data/ILINet.csv', colClasses=c("character","NULL","integer","integer","numeric","numeric","integer","integer","NULL","integer","integer","integer","integer","integer","integer"))
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
		scale_color_manual(values=c("blue","red","green","magenta","black")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive (CoV) or Percent Weighted ILI")

ggsave(fignatcovili, file="figures/natcov_ili.pdf", width=8, height=5)

# =============================================================================
# Examine CDC clinical flu:
# =============================================================================

nrevssclinflu <- read.csv('data/NREVSSClinicalFlu.csv', colClasses=c("NULL","NULL","integer","integer","integer","integer","integer","numeric","numeric","numeric"))
nrevssclinflu <- as_tibble(nrevssclinflu)

nrevssclinfludf <- nrevssclinflu %>% 
	select(YEAR, WEEK, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	left_join(yearweekconversion, by=c("YEAR"="Year","WEEK"="Week")) %>%
	ungroup() %>% 
	select(Date, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	rename("WEEK"="Date")

fignatcovnrevssclinflu <- natcov %>% 
	left_join(nrevssclinfludf, by=c("Week"="WEEK")) %>% 
	rename(Flu_Clinical="PERCENTPOSITIVE") %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","Flu_Clinical"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","green","magenta","black")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive")

ggsave(fignatcovnrevssclinflu, file="figures/natcov_nrevssclinflu.pdf", width=8, height=5)

# =============================================================================
# Examine CDC public health lab flu:
# =============================================================================

nrevssphflu <- read.csv('data/NREVSSPublicHealthFlu.csv', colClasses=c("NULL","NULL","integer","integer","integer","integer","integer","integer","integer","integer","integer","integer"))
nrevssphflu <- as_tibble(nrevssphflu)

nrevssphfludf <- nrevssphflu %>% 
	mutate(PERCENTPOSITIVE=(A2009H1N1 + AH3 + ANotSubtyped + B + BVic + BYam + H3N2v)/TOTALSPECIMENS) %>% 
	select(YEAR, WEEK, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	left_join(yearweekconversion, by=c("YEAR"="Year","WEEK"="Week")) %>%
	ungroup() %>% 
	select(Date, PERCENTPOSITIVE, TOTALSPECIMENS) %>% 
	rename("WEEK"="Date")

fignatcovnrevssphflu <- natcov %>% 
	left_join(nrevssphfludf, by=c("Week"="WEEK")) %>% 
	rename(Flu_PublicHealth="PERCENTPOSITIVE") %>% 
	pivot_longer(c("CoVHKU1","CoVNL63","CoVOC43","CoV229E","Flu_PublicHealth"), names_to="Virus", values_to="PercentPositive") %>% 
	ggplot(aes(x=Week, y=PercentPositive, col=Virus)) + 
		scale_color_manual(values=c("blue","red","green","magenta","black")) + 
		geom_point(alpha=0.8, size=0.5) +
		geom_line(alpha=0.8, size=1) + 
		theme_minimal() +
		labs(x="", y="Percent Positive")

ggsave(fignatcovnrevssphflu, file="figures/natcov_nrevssphflu.pdf", width=8, height=5)
