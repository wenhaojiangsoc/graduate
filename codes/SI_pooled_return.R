library(fastDummies)
library(ggplot2)
library(tidyr)
library(haven)
library(dplyr)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Prerequisite Functions for Decomposition ######
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

## endowment calculation
dummy_endowment <- 
  function(data, columns, weight) {
    for (i in 1:length(columns)){
      endowment_i <-
        dummy_cols(data, select_columns = columns[i]) %>%
        dplyr::select(starts_with(paste0(columns[i],"_")), weight) %>%
        summarise(across(everything(), ~weighted.mean(.x, weights = weight))) %>%
        dplyr::select(-weight) 
      if (i == 1){
        endowment <- endowment_i
      } else {
        endowment <- cbind(endowment, endowment_i) 
      }
    }
    return(endowment)
  }

## normalize coefficient
normalize_coefficient <-
  function(data_reg, data_weight, dv, columns, normal_weight=TRUE) {
    
    ## factor variables
    variables <- c()
    for (c in 1:length(columns)){
      variables[c] <- paste0("factor(",columns[c],")")
    }
    
    ## formula
    formula <- as.formula(paste(dv,
                                paste(variables, collapse = " + "),
                                sep = " ~ "))
    
    ## OLS model
    model <- lm(formula,
                data_reg, 
                weights=data_reg$weight)
    
    ## coefficient and normalize
    estimate <-
      data.frame(summary(model)$coef) %>%
      dplyr::select(Estimate) %>%
      rename(coef=Estimate)
    estimate$varname <- rownames(data.frame(summary(model)$coef))
    rownames(estimate) <- c()
    estimate$varname <- gsub("factor", "", estimate$varname)
    estimate$varname <- gsub("\\(", "", estimate$varname)
    estimate$varname <- gsub(")", "_", estimate$varname)
    estimate <- estimate %>% filter(varname!="Intercept_")
    estimate$vargroup <- sub('_[^_]*$', '', estimate$varname)
    estimate <- estimate %>% group_by(vargroup) %>% 
      group_modify(~ add_row(.x,.before=0)) %>%
      mutate(varname = case_when(
        is.na(varname) ~ paste0(vargroup,"_0"),
        .default = varname
      )) %>%
      mutate(coef = case_when(
        is.na(coef) ~ 0,
        .default = coef
      ))
    
    ## use specific year endowment
    endowment <-
      dummy_endowment(data_weight, columns=columns,
                      weight)
    estimate <-
      endowment %>% pivot_longer(cols = colnames(endowment), 
                                 names_to = "variable", 
                                 values_to = "value") %>%
      merge(estimate, by.x = "variable", by.y = "varname")
    
    if (normal_weight==TRUE) {
      estimate <- estimate %>%
        group_by(vargroup) %>%
        mutate(coef_normalize = coef - sum(coef*value)) %>%
        ungroup() %>%
        dplyr::select(variable,coef_normalize)
    } else {
      estimate <- estimate %>%
        group_by(vargroup) %>%
        mutate(n = n()) %>%
        mutate(coef_normalize = coef - sum(coef*1/n)) %>%
        ungroup() %>%
        dplyr::select(variable,coef_normalize)
    }
    
    return(estimate)
  }

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##### Functions for Longitudinal Decomposition ######
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

kim_decomposition_longitudinal <-
  function(
    data, year_start, year_end, dv, iv, decomp_var, ## decomp_var has to have value 1 and 2 (not 0 and 1)
    normal_weight=TRUE
  ) {
    
    #~#~#~#~#~#~#~#~#~
    ####### D2 #######
    #~#~#~#~#~#~#~#~#~
    
    ## get change of slopes
    coef_normalize <- normalize_coefficient(data_reg = data %>% filter(year==year_start,get(decomp_var)==2), 
                                            data_weight = data %>% filter(year==year_end|year==year_start),
                                            dv = dv,
                                            columns = iv,
                                            normal_weight = normal_weight)
    
    change_slope <-
      ((normalize_coefficient(data_reg = data %>% filter(year==year_end), 
                              data_weight = data %>% filter(year==year_end|year==year_start),
                              dv = dv,
                              columns = iv,
                              normal_weight = normal_weight) %>% dplyr::select(coef_normalize) - 
          normalize_coefficient(data_reg = data %>% filter(year==year_start), 
                                data_weight = data %>% filter(year==year_end|year==year_start),
                                dv = dv,
                                columns = iv,
                                normal_weight = normal_weight) %>% dplyr::select(coef_normalize)) - 
         (normalize_coefficient(data_reg = data %>% filter(year==year_end,get(decomp_var)==1), 
                                data_weight = data %>% filter(year==year_end|year==year_start),
                                dv = dv,
                                columns = iv,
                                normal_weight = normal_weight) %>% dplyr::select(coef_normalize) - 
            normalize_coefficient(data_reg = data %>% filter(year==year_start,get(decomp_var)==1), 
                                  data_weight = data %>% filter(year==year_end|year==year_start),
                                  dv = dv,
                                  columns = iv,
                                  normal_weight = normal_weight) %>% dplyr::select(coef_normalize)))
    change_slope$variable <- coef_normalize$variable
    
    ## get mean of endowment
    endowment <-
      dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==1), 
                      columns=iv,
                      weight)
    
    mean_endowment <- 
      1/4*(dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==2), columns=iv,
                           weight) %>% pivot_longer(cols = colnames(endowment), 
                                                    names_to = "variable", 
                                                    values_to = "value") %>%
             arrange(variable) %>% dplyr::select(value) + 
             dummy_endowment(data %>% filter(year==year_start&get(decomp_var)==2), columns=iv,
                             weight) %>% pivot_longer(cols = colnames(endowment), 
                                                      names_to = "variable", 
                                                      values_to = "value") %>%
             arrange(variable) %>% dplyr::select(value) +
             dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==1), columns=iv,
                             weight) %>% pivot_longer(cols = colnames(endowment), 
                                                      names_to = "variable", 
                                                      values_to = "value") %>%
             arrange(variable) %>% dplyr::select(value) +
             dummy_endowment(data %>% filter(year==year_start&get(decomp_var)==1), columns=iv,
                             weight) %>% pivot_longer(cols = colnames(endowment), 
                                                      names_to = "variable", 
                                                      values_to = "value") %>%
             arrange(variable) %>% dplyr::select(value))
    mean_endowment$variable <- coef_normalize$variable
    
    ## get total change
    change <- (weighted.mean(data[data$year==year_end & data[[decomp_var]] == 2,dv],
                             data[data$year==year_end & data[[decomp_var]] == 2,"weight"]) - 
                 weighted.mean(data[data$year==year_end & data[[decomp_var]] == 1,dv],
                               data[data$year==year_end & data[[decomp_var]] == 1,"weight"])) - 
      (weighted.mean(data[data$year==year_start & data[[decomp_var]] == 2,dv],
                     data[data$year==year_start & data[[decomp_var]] == 2,"weight"]) - 
         weighted.mean(data[data$year==year_start & data[[decomp_var]] == 1,dv],
                       data[data$year==year_start & data[[decomp_var]] == 1,"weight"])) 
    
    
    ## absolute change and percent being explained by each component
    D2_result <- data.frame(change = change_slope[,1] * mean_endowment[,1],
                            percent = change_slope[,1] * mean_endowment[,1] / change * 100,
                            variable = coef_normalize$variable)
    D2_result$component <- "D2"
    
    #~#~#~#~#~#~#~#~#~
    ####### D4 #######
    #~#~#~#~#~#~#~#~#~
    
    ## get mean of slope
    mean_slope <-
      1/4*((normalize_coefficient(data_reg = data %>% filter(year==year_end), 
                                  data_weight = data %>% filter(year==year_end|year==year_start),
                                  dv = dv,
                                  columns = iv,
                                  normal_weight = normal_weight) %>% dplyr::select(coef_normalize) + 
              normalize_coefficient(data_reg = data %>% filter(year==year_start), 
                                    data_weight = data %>% filter(year==year_end|year==year_start),
                                    dv = dv,
                                    columns = iv,
                                    normal_weight = normal_weight) %>% dplyr::select(coef_normalize)) +
             (normalize_coefficient(data_reg = data %>% filter(year==year_end,get(decomp_var)==1), 
                                    data_weight = data %>% filter(year==year_end|year==year_start),
                                    dv = dv,
                                    columns = iv,
                                    normal_weight = normal_weight) %>% dplyr::select(coef_normalize) + 
                normalize_coefficient(data_reg = data %>% filter(year==year_start,get(decomp_var)==1), 
                                      data_weight = data %>% filter(year==year_end|year==year_start),
                                      dv = dv,
                                      columns = iv,
                                      normal_weight = normal_weight) %>% dplyr::select(coef_normalize)))
    mean_slope$variable <- coef_normalize$variable
    
    ## change of endowment
    endowment <-
      dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==1), columns=iv,
                      weight)
    
    change_endowment <- 
      (dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==2), columns=iv,
                       weight) %>% pivot_longer(cols = colnames(endowment), 
                                                names_to = "variable", 
                                                values_to = "value") %>%
         arrange(variable) %>% dplyr::select(value) - 
         dummy_endowment(data %>% filter(year==year_start&get(decomp_var)==2), columns=iv,
                         weight) %>% pivot_longer(cols = colnames(endowment), 
                                                  names_to = "variable", 
                                                  values_to = "value") %>%
         arrange(variable) %>% dplyr::select(value)) -
      (dummy_endowment(data %>% filter(year==year_end&get(decomp_var)==1), columns=iv,
                       weight) %>% pivot_longer(cols = colnames(endowment), 
                                                names_to = "variable", 
                                                values_to = "value") %>%
         arrange(variable) %>% dplyr::select(value) -
         dummy_endowment(data %>% filter(year==year_start&get(decomp_var)==1), columns=iv,
                         weight) %>% pivot_longer(cols = colnames(endowment), 
                                                  names_to = "variable", 
                                                  values_to = "value") %>%
         arrange(variable) %>% dplyr::select(value))
    change_endowment$variable <- coef_normalize$variable
    
    ## percent being explained by each component
    D4_result <-
      data.frame(change = change_endowment[,1] * mean_slope[,1],
                 percent = change_endowment[,1] * mean_slope[,1] / change * 100,
                 variable = coef_normalize$variable)
    
    ## return results
    D4_result$component <- "D4"
    result <- rbind(D2_result,D4_result)
    return(result)
  }
