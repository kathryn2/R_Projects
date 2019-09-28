# Set CRAN Mirror
options(repos = c(CRAN = "http://cran.rstudio.com"))

# Set time zone
options(tz="America/New_York")

# Load data set in R using "Import Dataset" function
head(Football_Caps)

# Load needed libraries
library(collapsibleTree)

collapsibleTree(Football_Caps,
            hierarchy = c("Division","Region","Team"),
            width = 500)

library(dplyr)

Football_Caps %>%
  group_by(Division, Region, Team) %>%
  summarize(`Salary Spending` = sum(Cap_Spend)) %>%
collapsibleTreeSummary(
  hierarchy = c("Division","Region","Team"),
  root = "Football_Caps",
  width = 500,
  attribute = "Salary Spending"
)


divisionColors <- RColorBrewer::brewer.pal(length(unique(Football_Caps$Division)), "Blues")
# Regions will be a gradient that resets between divisions
regionColors <- Football_Caps %>%
  arrange(Division, Region) %>% 
  group_by(Division) %>%
  distinct(Region) %>%
  mutate(colors = colorspace::sequential_hcl(length(Region))[seq_along(Region)])
# Teams will also be a gradient that resets between divisions, but not Regions
teamColors <- Football_Caps %>%
  arrange(Division, Region) %>% 
  group_by(Division) %>%
  distinct(Team) %>%
  mutate(colors = colorspace::sequential_hcl(length(Team))[seq_along(Team)])

Football_Caps %>%
  arrange(Division, Region, Team) %>%
  collapsibleTree(
    hierarchy = c("Division", "Region", "Team"),
    root = "Football_Caps",
    width = 500,
    fill = c(divisionColors, regionColors$colors, teamColors$colors)
  )






Football_Caps %>%
  group_by(Division, Region, Team) %>%
  summarize(`Salary Spending $M` = sum(Spend)) %>%
  collapsibleTreeSummary(
    hierarchy = c("Division","Region","Team"),
    root = "Football_Caps",
    width = 500,
    attribute = "Salary Spending $M"
  )
