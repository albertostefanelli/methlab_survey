
options(scipen=999)

# PACKAGE INSTALLING AND UPGRADING #
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    )
  sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
  'tidyverse',
  "summarytools",
  'qualtRics',
  "gtsummary")

ipak(list_of_required_pkg)
# Q1 Consent 
# Q2 What is your position within the Faculty of Social Sciences?
# Q3 Would you define yourself as a:
# Q4 On a scale from 1 (not at all interested) to 4 (very interested), to what extent would you be interested in following a MethLab session on the following topics?
system("ls -t")
survey_data <- qualtRics::read_survey("data_numeric.csv")
survey_data_labels <- qualtRics::read_survey("data.csv")

names(survey_data)

survey_data$Q3 <- survey_data_labels$Q3

survey_data <- survey_data %>% mutate_at(vars(starts_with("Q4_")), as.numeric)

survey_data_labels %>% select(starts_with("Q4_"))

interested_quali <- grep("Q4_",names(survey_data))

quali_questions_labels <- c(
"Qualitative interviewing",
"Process tracing",
"Discourse analysis",
"Narrative analysis",
"Ethnographic and other field research methods",
"Diary study", 
"Qualitative data analysis",
"Historical methods",
"Multi-method research",
"Case-based research and set-theoretic thinking (QCA)",
"Data visualisation"
)


# for (i in seq(interested_quali)){
# q <- names(survey_data)[interested_quali[[i]]]
# n <- quali_questions_labels[[i]]
# print(tbl_cross(survey_data_labels, row=q, col=Q3, percent="row",
#                 label = list(q ~ n )
# ))
#   
# }

interested_quanti <- grep("Q5_",names(survey_data))

quanti_questions_labels <- c("Multi-level analysis",
"Structural Equation Modelling",
"Mediation analysis",
"Longitudinal modelling",
"Survival analysis",
"Logistic Regression and General Linear Models", 
"Survey and questionnaire design",
"Automated collection of web and social data",
"Network analysis",
"Machine learning",
"Quantitative text analysis",
"Panel data analysis",
"Bayesian analysis",
"Causal inference: experimental and semi-experimental design"
)


# for (i in seq(interested_quanti)){
#   q <- names(survey_data)[interested_quanti[[i]]]
#   n <- quanti_questions_labels[[i]]
#   print(tbl_cross(survey_data_labels, row=q, col=Q3, percent="row",
#                   label = list(q ~ n )
#   ))
#   
# }




