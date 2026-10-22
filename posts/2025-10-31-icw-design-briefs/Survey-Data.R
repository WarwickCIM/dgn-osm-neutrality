rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

survey <- read_excel("Equitable+cartography+workshop_October+21,+2025_08.39.xlsx")

vars <- names(survey)
feelings <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree")

survey_fac <- survey %>%
  mutate(
    across(
      all_of(vars),
      ~factor(.x, levels = c("Strongly agree", "Somewhat agree", "Neither agree nor disagree", 
                             "Somewhat disagree", "Strongly disagree"), ordered = TRUE)
    )
  )

survey_plot <- function(data, variable, title) {
  ggplot(data, aes(x={{variable}})) +
    geom_bar() +
    scale_x_discrete(drop=FALSE) +
    scale_y_continuous(
      limits = c(0, 17),
      breaks = seq(0, 15, by = 5)
    ) +
    labs(x=NULL, y="Frequency", title = title) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}

neutral <- survey_plot(survey_fac, mapsAreNeutral, "After having attended this workshop,\n I think maps are neutral.")
objective <- survey_plot(survey_fac, mapsAreObjective, "After having attended this workshop,\n I think maps are objective.")
considerEDI <- survey_plot(survey_fac, considerEDI, "After having attended this workshop,\n it's important to consider EDI in creating maps.")
equitable <- survey_plot(survey_fac, mapsAreEquitable, "After having attended this workshop,\n current maps are equitable.")
diverse <- survey_plot(survey_fac, diverseInfoImportant, "This workshop made me more aware about the importance\n of incorporating diverse information on a map.")
understanding <- survey_plot(survey_fac, renewedUnderstanding, "This workshop provided me with renewed understanding\n of the relationship between maps, inequalities and neutrality.")
roleEDI <- survey_plot(survey_fac, mapsRoleEDI, "This workshop gave me an opportunity to reflect and discuss\n about the role of maps to promote/hamper equity and inclusion.")
interestingPeople <- survey_plot(survey_fac, metInterestingPeople, "This workshop made me met interesting people.")
fun <- survey_plot(survey_fac, fun, "This workshop was fun.")
useful <- survey_plot(survey_fac, useful, "This workshop was useful.")
interesting <- survey_plot(survey_fac, interesting, "This workshop was interesting.")

(fun + useful + interesting) + 
  plot_annotation(
    title="Impressions of the Workshop",
    theme=theme(plot.title=element_text(size=20, hjust=0.5, face="bold"))
)

((diverse + understanding)/(roleEDI + interestingPeople)) + 
  plot_annotation(
    title="Impacts of the Workshop",
    theme=theme(plot.title=element_text(size=20, hjust=0.5, face="bold"))
  )

((neutral + objective)/(considerEDI + equitable)) + 
  plot_annotation(
    title="Impressions of Maps Post-Workshop",
    theme=theme(plot.title=element_text(size=20, hjust=0.5, face="bold"))
  )
