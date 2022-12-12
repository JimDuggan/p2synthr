library(deSolve)
library(dplyr)

DT <- 0.25
PULSE_CHANGE <- 0.01
cascades <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    
    # Variant 1 to variant 2 progression
    p_var_01_to_var_02 <- 0
    if(time==st_02){
      # cat("Pulse v_01 to v_02 time = ", time,"\n")
      p_var_01_to_var_02 <- PULSE_CHANGE/DT
    }
    
    
    # Variant 2 to variant 3 progression
    p_var_02_to_var_03 <- 0
    if(time==st_03){
      # cat("Pulse v_02 to v_03 time = ", time,"\n")
      p_var_02_to_var_03 <- PULSE_CHANGE/DT
    }
    
    # Variant 3 to variant 4 progression
    p_var_03_to_var_04 <- 0
    if(time==st_04){
      # cat("Pulse v_03 to v_04 time = ", time,"\n")
      p_var_03_to_var_04 <- PULSE_CHANGE/DT
    }
    
    # Variant 4 to variant 5 progression
    p_var_04_to_var_05 <- 0
    if(time==st_05){
      # cat("Pulse v_04 to v_05 time = ", time,"\n")
      p_var_04_to_var_05 <- PULSE_CHANGE/DT
    }
    


    ddt_V01 <- -p_var_01_to_var_02 - gr_02 * Var_01 * Var_02
    
    ddt_V02 <-  p_var_01_to_var_02 + gr_02 * Var_01 * Var_02 -
                p_var_02_to_var_03 - gr_03 * Var_02 * Var_03
    
    ddt_V03 <-  p_var_02_to_var_03 + gr_03 * Var_02 * Var_03 -
                p_var_03_to_var_04 - gr_04 * Var_03 * Var_04
    
    ddt_V04 <- p_var_03_to_var_04 + gr_04 * Var_03 * Var_04 -
               p_var_04_to_var_05 - gr_05 * Var_04 * Var_05
      
    ddt_V05 <- p_var_04_to_var_05 + gr_05 * Var_04 * Var_05

  
    return (list(c(ddt_V01,
                   ddt_V02,
                   ddt_V03,
                   ddt_V04,
                   ddt_V05)))  
  })
}

run_sim <- function(duration,
                    start_times,
                    growth_rates,
                    variant_names)
{
  simtime <- seq(1,duration,by=DT)
  stocks  <- c(Var_01=1.0,
               Var_02=0.0,
               Var_03=0.0,
               Var_04=0.0,
               Var_05=0.0)
  
  
  auxs <- c(start_times, growth_rates)
  
  # cat("Params = ",auxs,"\n")
  
  o<-as_tibble(data.frame(deSolve::ode(y=stocks, 
                                   times=simtime, 
                                   func = cascades, 
                                   parms=auxs, 
                                   method="euler")))
  
  o1 <- o %>% dplyr::filter(time %% 1 == 0) %>%
          dplyr::mutate(CheckSum=Var_01+Var_02+Var_03+Var_04+Var_05)
  
  o1
}


#' \code{synth_variants} 
#' Generates cases for five successive variants
#'
#' @param cases_ts time series of cases
#' @param the_dates the actual dates of teh time series
#' @param percent_genomic_surveillance_min the min percentage of cases that are sequenced
#' @param percent_genomic_surveillance_max the max percentage of cases that are sequenced
#' @param variant_start_time_fractions The time fraction for the start of var2-var5
#' @param variant_growth_rates the growth rate for each variant
#' @param variant_names the variant names
#' @param setSeed allows setting of the seed for sampling
#' @param seedValue set the seed value to be used if needed
#' @return tibble output data
#'         
#' @export
#'
synth_variants <- function(cases_ts,
                           the_dates,
                           percent_genomic_surveillance_min=0.5,
                           percent_genomic_surveillance_max=0.8,
                           variant_start_time_fractions = c(st_02=0.10,
                                                            st_03=0.30,
                                                            st_04=0.50,
                                                            st_05=0.70),
                           variant_growth_rates = c(gr_02=0.10,
                                                    gr_03=0.20,
                                                    gr_04=0.30,
                                                    gr_05=0.05),
                           variant_names=c(name_01="Wuhan",
                                           name_02="Alpha",
                                           name_03="Delta",
                                           name_04="Omicron1",
                                           name_05="Omicron2"),
                           setSeed=F,
                           seedValue=100){
  
  time=length(cases_ts)
                             
  variant_start_times <- round(time* variant_start_time_fractions,0)

  vars <- run_sim(duration=length(cases_ts), 
                  start_times = variant_start_times,
                  growth_rates = variant_growth_rates,
                  variant_names = variant_names)
  
  seq_proportion <- round(runif(length(cases_ts),
                                percent_genomic_surveillance_min,
                                percent_genomic_surveillance_max),2)
  
  col_1 <- paste0("SynCases",unname(variant_names["name_01"]))
  col_2 <- paste0("SynCases",unname(variant_names["name_02"]))
  col_3 <- paste0("SynCases",unname(variant_names["name_03"]))
  col_4 <- paste0("SynCases",unname(variant_names["name_04"]))
  col_5 <- paste0("SynCases",unname(variant_names["name_05"]))
  col_6 <- "SynCasesUnsequenced"
  
  Total_Var_01 <- round(seq_proportion * vars$Var_01 * cases_ts,0)
  Total_Var_02 <- round(seq_proportion * vars$Var_02 * cases_ts,0)
  Total_Var_03 <- round(seq_proportion * vars$Var_03 * cases_ts,0)
  Total_Var_04 <- round(seq_proportion * vars$Var_04 * cases_ts,0)
  Total_Var_05 <- round(seq_proportion * vars$Var_05 * cases_ts,0)
  
  TotalSequenced <- Total_Var_01 +
                    Total_Var_02 +
                    Total_Var_03 +
                    Total_Var_04 +
                    Total_Var_05
  
  
  final <- vars %>%
            dplyr::select(-CheckSum) %>%
            dplyr::mutate(TotalCases = cases_ts,
                          SequencingFraction=seq_proportion,
                          SequencedCases=TotalSequenced,
                          !!col_1 := Total_Var_01,
                          !!col_2 := Total_Var_02,
                          !!col_3 := Total_Var_03,
                          !!col_4 := Total_Var_04,
                          !!col_5 := Total_Var_05,
                          !!col_6 :=TotalCases - TotalSequenced) %>%
            mutate(Date=the_dates) %>%
            select(time,Date,everything())
  
  names(final)[3:7] <- paste0(unname(variant_names),"Fr")
    
  
  final

}
                                                