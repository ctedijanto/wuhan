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
	geom_point(alpha=0.8, size=0.5) +
	geom_line(alpha=0.8, size=1) + 
	theme_minimal() 

ggsave(fignatcov, file="~/DropboxHarvard/Projects/Wuhan/natcov.pdf", width=7, height=5)

