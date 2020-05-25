#' ---
#' title: "US model"
#' date: "`r Sys.time()`"
#' author: Claudio Zanettini
#' categories:
#'   - covid-19
#' tags: 
#'   - covid-19
#'   - Italy
#' lastmod: "`r Sys.time()`"
#' keywords: []
#' description: ''
#' output:
#'   html_document:
#'     df_print: kable
#'     toc: true
#'     theme: journal
#' ---
#' 
#' # PP score analysis
#' 
#' ## Get the data
#' 
#' This is to get the data and clean them up.
#' 
## ----get_data3, message=FALSE, warning=FALSE--------------------------------------------------------------------
# libraries --------------
# devtools::install_github("zeehio/facetscales")
library(facetscales)
library(covid19census)
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(gganimate)
library(ggcorrplot)
library(broom)
library(purrr)
library(knitr)
library(kableExtra)
library(htmlwidgets)
library(plotly)
library(stringr)
theme_set(theme_bw())

# var new to select -------
to_select<- c("date", "county", "state", "fips", "cases", "deaths", "perc_families", 
"perc_family_only_onep", "perc_edu_associate", "perc_edu_bachelor", 
"perc_withinternet", "total_pop", "perc_imm65", "total_imm65", 
"total_beds", "ratio_beds", "urban", "perc_acute_myocardial_infarction", 
"perc_alzheimer_dementia", "perc_asthma", "perc_atrial_fibrillation", 
"perc_cancer_breast", "perc_cancer_colorectal", "perc_cancer_lung", 
"perc_ch_obstructive_pulm", "perc_chronic_kidney_disease", "perc_depression", 
"perc_diabetes", "perc_heart_failure", "perc_hypertension", "perc_ischemic_heart_disease", "perc_obesity", "perc_osteoporosis", "perc_rheumatoid_arthritis", 
"perc_schizophrenia_psychotic_dis", "perc_stroke", "perc_tobacco_use", 
"median_income", "pm2.5", "summer_temp", "summer_hum", "winter_temp", 
"winter_hum", "age65_over", "median_age", "sex_ratio", "child_dependency", 
"perc_black", "perc_lat", "perc_white", "perc_asian", "perc_island", 
"perc_other", "perc_two_more_races", "total_tests", "days_f0"
)


# 
# var to select old -------
to_select <- c(
  "date",
  "county",
  "state",
  "fips",
  "cases",
  "deaths",
  "perc_families",
  "perc_familes_18childreen",
  "perc_married_couples",

  "perc_family_only_onep",
  # "perc_married_couples_u18ychildreen",
  # "perc_families_only_male",
  # "perc_families_only_male__18ychildreen",
  # "perc_families_only_female",
  # "perc_families_only_female__18ychildreen",
  # "perc_non_families_alone",
  # "perc_non_families_alone65y",
  # "perc_non_families_u18y",
  # "perc_non_families_65y",
  # "perc_relationship_householder",
  # "perc_relationship_spouse",
  # "perc_relationship_child",
  # "perc_relationship_other_relatives",
  # "perc_relationship_other_nonrelatives",
  # "perc_relationship_other_unmaried_part",
  # "perc_marital_status_male_nevermaried",
  # "perc_marital_status_male_maried",
  # "perc_marital_status_male_separated",
  # "perc_marital_status_male_widowed",
  # "perc_marital_status_male_divorced",
  # "perc_marital_status_female_nevermaried",
  # "perc_marital_status_female_maried",
  # "perc_marital_status_female_separated",
  # "perc_marital_status_female_widowed",
  # "perc_marital_status_female_divorced",
  "perc_enrolled_preschool",
  "perc_enrolled_kindergarden",
  "perc_enrolled_elementary",
  "perc_enrolled_highschool",
  "perc_enrolled_college",
  # "perc_edu_9grade",
  # "perc_edu_nodiploma",
  # "perc_edu_highshool",
  "perc_edu_somecollege",
  "perc_edu_associate",
  "perc_edu_bachelor",
  # "perc_edu_gradprofess",
  # "perc_edu_highshool_higher",
  # "perc_edu_batchelor_higher",
  # "perc_american",
  # "perc_arab",
  # "perc_czech",
  # "perc_danish",
  # "perc_dutch",
  # "perc_english",
  # "perc_french_except_basque",
  # "perc_french_canadian",
  # "perc_german",
  # "perc_greek",
  # "perc_hungarian",
  # "perc_irish",
  # "perc_italian",
  # "perc_lithuanian",
  # "perc_norwegian",
  # "perc_polish",
  # "perc_portuguese",
  # "perc_russian",
  # "perc_scotch_irish",
  # "perc_scottish",
  # "perc_slovak",
  # "perc_subsaharan_african",
  # "perc_swedish",
  # "perc_swiss",
  # "perc_ukrainian",
  # "perc_welsh",
  # "perc_west_indian_excluding_hispanic_origin_groups",
  # "perc_withcomputer",
  "perc_withinternet",
  "total_pop",
  "perc_imm65",
  "total_imm65",
  "total_beds",

  "ratio_beds",
  "urban",
  "perc_acute_myocardial_infarction",
  "perc_alzheimer_dementia",
  "perc_asthma",
  "perc_atrial_fibrillation",
  "perc_cancer_breast",
  "perc_cancer_colorectal",
  "perc_cancer_lung",
  "perc_cancer_all",
  "perc_ch_obstructive_pulm",
  "perc_chronic_kidney_disease",
  "perc_depression",
  "perc_diabetes",
  "perc_heart_failure",
  "perc_hypertension",
  "perc_ischemic_heart_disease",
  "perc_obesity",
  "perc_osteoporosis",
  "perc_rheumatoid_arthritis",
  "perc_schizophrenia_psychotic_dis",
  "perc_stroke",
  "perc_tobacco_use",
  # to many NA for these:
  # "urgent_admission",
  # "annual_wellness_visit",
  # "elective_admission",
  # "emergent_admission",
  # "other_admission",
  # perc_pneumococcal_vaccine HIGHLY CORRELATED WITH perc_imm65
  # "perc_pneumococcal_vaccine",
  # "perc_poverty",
  "median_income",
  "pm2.5",
  "summer_temp",
  "summer_hum",
  "winter_temp",
  "winter_hum",
  "age65_over",
  "median_age",
  # "median_age_male",
  # "median_age_female",
  "sex_ratio",
  # "age_dependency",
  # "old_age_dependency",
  "child_dependency",
  "perc_black",
  "perc_lat",
  "perc_white",

  "perc_asian",
  "perc_island",
  "perc_other",
  "perc_two_more_races",
  "total_tests",
  "days_f0"
)



# df1   -----------------------------
df1 <- getus_all()

# df2 ------------------------------
df2 <-
  df1 %>%
  # calculate age65_over and perc_black, white and latinos, total tests
  mutate(age65_over = rowSums(dplyr::select(., matches("perc_[6][5-9]|[7|8][0-9]")))) %>%
  mutate(urban = if_else(urban == "Urban", 1, 0)) %>%
  mutate(perc_black = total_black / total_pop * 100) %>%
  mutate(perc_white = total_white / total_pop * 100) %>%
  mutate(perc_lat = total_latino / total_pop * 100) %>%
  mutate(total_tests = positive + negative) %>%

  # calculate day since first case
  mutate(f_date = case_when(cases >= 1 ~ date)) %>%
  group_by(fips) %>%
  mutate(f_date = min(f_date, na.rm = TRUE), days_f0 = as.numeric(date - f_date)) %>%
  ungroup() %>%
  mutate(days_f0 = if_else(is.finite(days_f0), days_f0, NA_real_)) %>%
  
  # newly calculated-----------------------------------  
  # total beds in ratio
  mutate(ratio_beds = total_beds/total_pop) %>% 
  # asian are here
  mutate(perc_asian = total_asian / total_pop * 100) %>%
  mutate(perc_island = total_pacific_islander / total_pop * 100) %>%
  mutate(perc_native = total_native / total_pop * 100) %>%
  mutate(perc_other = total_other_race / total_pop * 100) %>%
  mutate(perc_two_more_races = total_two_more_races / total_pop * 100) %>%
  
  
  # family with one parent together
  mutate(perc_family_only_onep = perc_families_only_female + perc_families_only_male) %>% 
  mutate(total_imm65 = age65_over/100 *perc_imm65) %>% 
   
  # select variables ----------
  dplyr::select(!!to_select) %>%

  # perc divided by 100
  mutate_at(vars(starts_with("perc")), function(x) x / 100) %>%
  mutate(age65_over = age65_over / 100) %>%

  # we remove NAs
  na.omit() %>%

  # at least 1 death
  filter(deaths >= 1)


# all_dat -------------
all_dat <- df2 %>%
  # make variables numeric
  mutate_at(vars(
    -date, -county, -state, -fips), 
     as.numeric
  ) %>%
  # rename
  # add XX_ to all the variables excepts those
  rename_at(
    vars(
      -date, -county, -state, -fips, -cases, -deaths,
      -perc_imm65, -cases, -total_pop, -total_imm65
    ),
    ~ paste0("XX_", .)
  ) %>%
  rename_at(vars(perc_imm65, total_imm65), ~ paste0("ZZ_", .)) %>%
  rename_at(vars(deaths), ~ paste0("YY_", .)) %>%
  rename_at(vars(cases), ~ paste0("NC_", .)) %>%
  rename_at(vars(total_pop), ~ paste0("NP_", .)) %>%
  # calculate logitZZ
  mutate(logitZZ_perc_imm65 = log(ZZ_perc_imm65) - log(1 - ZZ_perc_imm65)) 
  # mutate(logitZZ_total_imm65 = log(ZZ_total_imm65) - log(1 - ZZ_total_imm65))
  

# today_dat ------------------------------
today_dat <- all_dat %>%
  filter(date == max(date))

#' 
#' 
#' #  Anlysis Functions
#' 
#' These is a set of formulas used to analyzed and graph the data. 
#' 
## ----functions--------------------------------------------------------------------------------------------------
get_PPScore <- function(
                        dat = .x, 
                        filt_cases = 1,
                        # formula to use for propensity score
                        # this is a `reformulate` expression 
                        form = form_ps, 
                        # the offset of glm function
                        offset_f = "log(NC_cases)",
                        var_dep =  "YY_deaths",
                        # variable of interest
                        var_int = "ZZ_perc_imm65",
                        # propensity score quantile
                        quant = FALSE,
                        include_model = FALSE
                        ){

# given a covid dataframe with 1 date, it calculate effect of the indipendent variable using a propensity score

  dat <- dat %>%
    filter(dat$NC_cases >= !!filt_cases)
  
  PropScoresLM <- lm(form, data = dat)
  
  if(quant == TRUE){
    dat[, "PP"] <- cut(fitted.values(PropScoresLM), 4, labels = FALSE)
  } else {
    dat[, "PP"] <- fitted.values(PropScoresLM)
  }
  
  form_glm <- reformulate(termlabels = c(var_int, "PP"), response = var_dep)

  outcome_PP <-
    # we add on the fly PP to the dataframe and we
    dat %>%
    glm(form_glm,
        family = quasipoisson(link = "log"),
        offset = eval(parse(text = offset_f)), 
        data = .
    )
  
  
  res <- tidy(outcome_PP) %>%
    filter(term == !!var_int) %>%
    mutate(
      deaths = sum(dat$NC_cases),
      n_counties = nrow(dat),
      urban = sum(dat$XX_urban),
      formula =  deparse(formula(outcome_PP)),
      offset_f = offset_f,
      quant = FALSE
    )
  
  if(include_model == TRUE) {
    res <- res %>% 
      mutate(model = list(outcome_PP))
  }
  
 message(
    cat(paste0(
      "\nDate ",
      first(dat$date2),
      ":\nApplying model ",
      first(res$formula),
      "\noffset: ",
      first(offset_f),
      "\nfilt_cases = ",
      filt_cases,
      "!"
    ))
 )
 res
}



glm_pp <- function(dat, 
                   dates_tostudy = dates_tostudy, 
                   offset_f = "log(NC_cases)",
                   var_dep,
                   var_int,
                   data_term = FALSE, 
                   quant = FALSE, 
                   include_model = FALSE,
                   ...){

# functions to apply glm pp model to diffenret dates in dataframe
# dat = covid dataframe 
# dates_tostudy = list of dates to perform the analysis on                    
# filt_cases =  numeric vector of threasholds of cases X county  (default = 1)
# form = formula to use for propensity score this is a `reformulate` expression for the pp score (form_ps)
# offset_f = the offset of glm function ("log(NC_cases)")
# var_dep =  dependent variable ("YY_death")
# var_int = variable of interest ("ZZ_perc_imm65"),
# quant = propensity score quantile (FALSE)

  suppressWarnings(
  zz_term <-   
  dat %>%
  # filter(YY_deaths > 0) %>% 
  filter(date %in% dates_tostudy) %>%
  mutate(date2 = date) %>% 
  nest(-date) %>%
  # we repeat the date slice X number of case threeshold that we want to study
  slice(rep(1:n(), each = length(to_filt_cases))) %>%
  mutate(filt_cases_c = rep(to_filt_cases, nrow(.)/length(to_filt_cases))) %>%
  
  mutate(fit = 
            map2(
            data,
            filt_cases_c,
            get_PPScore,
            form = form_ps, 
            offset_f = !!offset_f,
            quant = !!quant,
            var_int = !!var_int,
            var_dep = !!var_dep,
            include_model = !!include_model
            )
  ) %>% 
    unnest(fit))
    
  if(data_term == FALSE){
    zz_term_nd <- zz_term %>% 
    select(-data) 
    return(zz_term_nd)
  }else{
    return(zz_term)
    } 
}

#' 
#' 
#' ggplot graph
#' 
## ----ggcovid----------------------------------------------------------------------------------------------------

ggcovid <- function(dat, x_var) {
  
if(!x_var %in% c("date", "cases")) stop("x_var can be date or cases!")

if (x_var == "date"){
  to_add <- "; cases > 0" 
  aes_x <- "date"
  dat <- dat %>%
    filter(filt_cases_c == 1) 
  x_title = NULL
} 

if (x_var == "cases") {
  to_add <- paste0("; date ",  first(dat$date))
  aes_x <- "filt_cases_c"
  dat <- dat %>%
    filter(date == max(date))  
  x_title = "inclusion criteria (minimum cases)"
  
}

dep_var <- stringr::word(first(dat$formula), 1)
  
dat %>% 
  mutate(`urban/counties` = urban/n_counties) %>% 
  rename(`number of counties` = n_counties) %>% 
  pivot_longer(cols = 
                 c("estimate", "number of counties", "urban/counties"),
               names_to = "var_name") %>%
  mutate(sign = if_else(p.value < 0.05, "p<0.05", "NS")) %>%
  mutate(sign = factor(sign, levels = c("p<0.05", "NS"))) %>% 
  mutate(std.error = if_else(var_name == "estimate", std.error, 0)) %>%

  ggplot(aes_string(aes_x, "value", color = "sign", group = "term")) +
      geom_line() +
      geom_point() +
      geom_linerange(aes(ymax = value + std.error, ymin = value - std.error), show.legend = FALSE) +
      facet_grid(vars(var_name), scales = "free") +
      scale_color_manual(values = c("NS" = "#00BFC4", "p<0.05" = "#F8766D"), drop = FALSE) +
      theme(
        # legend.position = "top",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black")
      ) +
      labs(
        title = paste0("Slope of ", first(dat$term), " over time"),
        subtitle = paste0(dep_var, "; offset = ", first(dat$offset_f),
                          ";\nquantile PP = ", first(dat$quant), to_add),
        x = x_title
      ) +
  expand_limits(y = 0) 

}

#' 
#' ## Results
#' 
#' ### Deaths ~ perc_imm65 + PP, offset_f = "log(NP_total_pop)"
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_perc_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zz_term_np <- glm_pp(dat = all_dat,
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NP_total_pop)",
       var_dep = "YY_deaths",
       var_int = "ZZ_perc_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = TRUE
       )


zz_term_np_today  <- zz_term_np %>% 
  filter(date == max(date), filt_cases_c == 1) 

print(zz_term_np_today$model)

ggcovid(zz_term_np, "date")

#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zz_term_np, "cases")

#' 
#' 
#' ### Deaths ~ perc_imm65 + PP, offset_f = "log(NC_cases)"
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_perc_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zz_term_nc <- glm_pp(dat = all_dat,
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NC_cases)",
       var_dep = "YY_deaths",
       var_int = "ZZ_perc_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = FALSE
       )

zz_term_nc_today  <- zz_term_np %>% 
  filter(date == max(date), filt_cases_c == 1) 

print(zz_term_nc_today$model)

ggcovid(zz_term_nc, "date")

#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zz_term_nc, "cases")

#' 
#' ### NC_cases ~ perc_imm65 + PP, offset_f = "log(NP_total_pop)"
#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_perc_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zz_term_cases_nc <- glm_pp(dat = all_dat,
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NP_total_pop)",
       var_dep = "NC_cases",
       var_int = "ZZ_perc_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = FALSE
       )

zz_term_cases_nc_today  <- zz_term_np %>% 
  filter(date == max(date), filt_cases_c == 1) 

print(zz_term_cases_nc_today$model)


ggcovid(zz_term_cases_nc, "date")

## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zz_term_cases_nc, "cases")

#' 
#' 
#' ### No New York: Deaths ~ perc_imm65 + PP, offset_f = "log(NP_total_pop)"
#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_total_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zzterm_noNY_np  <- 
  all_dat %>% 
  filter(state !="New York") %>% 
      glm_pp(
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NP_total_pop)",
       var_dep = "YY_deaths",
       var_int = "ZZ_total_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = TRUE
       )

zzterm_noNY_np_today <- zzterm_noNY_np  %>% 
  filter(date == max(date), filt_cases_c == 1)

zzterm_noNY_np_today$model

ggcovid(zzterm_noNY_np, "date")

#' 
#' 
#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zzterm_noNY_np, "cases")

#' 
#' ### No New York: Deaths ~ perc_imm65 + PP, offset_f = "log(NC_cases)"
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_total_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zzterm_noNY_nc  <- 
  
  all_dat %>% 
  filter(state !="New York") %>% 
      glm_pp(
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NC_cases)",
       var_dep = "YY_deaths",
       var_int = "ZZ_total_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = TRUE
       )

zzterm_noNY_nc_today <- zzterm_noNY_np  %>% 
  filter(date == max(date), filt_cases_c == 1)

zzterm_noNY_nc_today$model

ggcovid(zzterm_noNY_nc, "date")

#' 
#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zzterm_noNY_nc, "cases")

#' 
#' 
#' ### Only New York: Deaths ~ perc_imm65 + PP, offset_f = "log(NP_total_pop)"
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_total_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zzterm_onlyNY_np  <- 
  
  all_dat %>% 
  filter(state =="New York") %>% 
      glm_pp(
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NP_total_pop)",
       var_dep = "YY_deaths",
       var_int = "ZZ_total_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = TRUE
       )

zzterm_onlyNY_np_today <- zzterm_noNY_np  %>% 
  filter(date == max(date), filt_cases_c == 1)

zzterm_onlyNY_np_today$model

ggcovid(zzterm_onlyNY_np, "date")

#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zzterm_onlyNY_np, "cases")

#' 
#' ### Only New York: Deaths ~ perc_imm65 + PP, offset_f = "log(NC_cases)"
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
XX <- grep("XX", names(today_dat), value = TRUE)
XX <- XX[!XX %in% c("XX_total_beds") ]

form_ps <- reformulate(termlabels = XX, response = "ZZ_total_imm65")

dates_tostudy <- sort(unique(all_dat$date))[52:length(unique(all_dat$date))]
to_filt_cases  <- c(1, seq(10, 100, 10))   


zzterm_onlyNY_nc  <- 
  
  all_dat %>% 
  filter(state =="New York") %>% 
      glm_pp(
       dates_tostudy = dates_tostudy,
       filt_cases = to_filt_cases,
       form = form_ps, 
       offset_f = "log(NC_cases)",
       var_dep = "YY_deaths",
       var_int = "ZZ_total_imm65",
       quant = FALSE,
       data_term = FALSE,
       include_model = TRUE
       )

zzterm_onlyNY_nc_today <- zzterm_noNY_np  %>% 
  filter(date == max(date), filt_cases_c == 1)

zzterm_onlyNY_nc_today$model


ggcovid(zzterm_onlyNY_nc, "date")

#' 
#' 
## ----fig.width=4, fig.height=4----------------------------------------------------------------------------------
ggcovid(zzterm_onlyNY_nc, "cases")

#' 
#' 
#' 
#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------


#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
