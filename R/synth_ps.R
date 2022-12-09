library(dplyr)
library(lubridate)
library(purrr)


#' \code{synth_ps} 
#' Takes incidence and generates a noisy leading indicator to model
#' participatory surveillance
#'
#' @param weekly_cases weekly reported cases
#' @param population_size population size
#' @param reporting_fraction estimated reporting fraction for cases
#' @param proportion_enrolled proportion enrolled in participatory surveillance
#' @param active_users_lower lower proportion of active users
#' @param active_users_higher higher proportion of active users
#' @param fp_fraction fraction false positives
#' @param set_seed flag for setting the seed
#' @param SEED value for the seed
#' @param rbinomial_size dispersion parameter for negative binomial sample.
#' @return tibble with synthetic participatory surveillance numbers
#' @export
#' 
synth_ps <- function(weekly_cases,
                     dates,
                     population_size,
                     reporting_fraction=0.50,
                     proportion_enrolled=0.0025,
                     leading_time=1,
                     false_positive_fraction=0.10,
                     set_seed=F,
                     SEED=100,
                     rbinomial_size=5,
                     active_users_lower=0.5,
                     active_users_upper=0.90)
{
  if(set_seed)
    set.seed(SEED)
  
  L <- length(weekly_cases)
  total_enrolled <- round(population_size*proportion_enrolled,0)
  weekly_active_users <- round(total_enrolled * runif(L,active_users_lower,active_users_upper),0)
  total_estimated_cases <- round(weekly_cases/reporting_fraction,0)
  self_reported_cases <- (total_estimated_cases / population_size) * weekly_active_users * 
                            (1+false_positive_fraction)
  self_reported_cases <- round(self_reported_cases,0)
  self_reported_cases_lead <- dplyr::lead(self_reported_cases,leading_time)
  
  self_reported_cases_lead_noise <- purrr::map_dbl(self_reported_cases_lead,
                                            ~rnbinom(n=1,
                                                     mu=.x,
                                                     size=rbinomial_size))
  
  tibble::tibble(Index=1:L,
         Date=dates,
         Year=year(Date),
         Week=week(Date),
         WeeklyCases=weekly_cases,
         ReportingFraction=rep(reporting_fraction,L),
         EstimatedCasesDet=total_estimated_cases,
         Population=rep(population_size,L),
         ProportionEnrolled=rep(proportion_enrolled,L),
         LeadingTime=rep(leading_time,L),
         FPFraction=rep(false_positive_fraction,L),
         TotalEnrolled=total_enrolled,
         WeeklyActiveUsers=weekly_active_users,
         PSReportedCasesDet=self_reported_cases_lead,
         PSSyntheticReported= self_reported_cases_lead_noise)
    
  
}
