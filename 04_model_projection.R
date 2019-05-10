# ---------------------------------------------------------------------------- #
#
# Model actuel                                                            ----
#
# ---------------------------------------------------------------------------- #

# Cette fonction identifie une serie particuliere (ex: "ON-DI--AUTO-CCC") et la projette dans le temps

## Etapes: 

# Step 0 : Identification de la serie
# Step 1 : Creation d'un train et d'un test
# Step 2 : Modele GLM Poisson
# Step 3 : Modele avec auto.arima()
# Step 4 : Modele ARIMAX
# Step 5 : Modele STL
# Step 6 : Comparaison MAPE pour trouver le meilleur modele
# Step 7 : Prediction avec le meilleur model sur le dataset complet (train + valid)
# Step 8 : Formater les resultats sous un format specifique

## Parametres :

# - version:    pour comparer les projections d'un mois sur l'autre.
# - shiny:      si FALSE, les messages s'affichent avec print()
#               si TRUE, les messages s'affichent avec withProgress()
# - y:          observations.
#               NBVENTP ---> ventes 
#               NBSOUMP ---> soumissions
# - date.start: debut de la serie (on peut repousser le debut si le depart de la serie est instable).
# - date.proj:  jusqu'a quelle date projeter la serie.
# - date.valid: a quelle date separe-t-on le data complet en 2 (train et test).
# - w:          si on veut mettre des poids dans le modele.
# - best:       si on veut fixer à l'avance le meilleur modele.
# - y_min:      si l'on ne veut pas considerer certaines observations trop basses.
# - y_max:      si l'on ne veut pas considerer certaines observations trop hautes.



model.serie <- function(
  serieName, 
  data = y.all, 
  shiny = F, 
  version = '2018-12', 
  y = 'NBVENTP', 
  y_min = 0, 
  y_max = 1e6,
  date.start = min(data$LUNDI), 
  date.proj = '2019-12-30', 
  date.valid = NULL,
  family = 'poisson', 
  w = NULL, 
  best = NULL
)
{

  # -------------------------------------------------------------------------- #
  # *** 0. Identification                                                 ----
  # -------------------------------------------------------------------------- #
  
  printProgress(paste0("Serie: ", serieName), shiny, {
    split <- unlist(strsplit(serieName, "-"))
    serie1 <- filter(data, PROVINCE == split[1] & BANNER == split[2])
    if(length(split) > 2 & split[3] != ""){ serie1 <- filter(serie1, GROUP        == split[3]) }
  if(length(split) > 3 & split[4] != ""){ serie1 <- filter(serie1, PRODUCT      == split[4]) }
    if(length(split) > 4 & split[5] != ""){ serie1 <- filter(serie1, ORIGINE_SOUM == split[5]) }
  })
  
  # -------------------------------------------------------------------------- #
  # *** 1. train/test                                                     ----
  # -------------------------------------------------------------------------- #
  
  printProgress("Step 1/8: train & test datasets", shiny, {
    if(is.null(date.valid))
      date.valid <- last(serie1$LUNDI) - 9*30
    
    train <- serie1 %>%
      group_by(LUNDI, ANOP, SEMOP) %>% 
      summarise_at(vars(one_of(y)), funs(sum)) %>%
      filter(LUNDI >= date.start & LUNDI <= date.valid) %>% 
      left_join(x.all, by = c('LUNDI', 'ANOP', 'SEMOP')) %>% 
      as.data.frame
    
    date.start <- min(train$LUNDI)
    
    valid <- serie1 %>%
      group_by(LUNDI, ANOP, SEMOP) %>% 
      summarise_at(vars(one_of(y)), funs(sum)) %>%
      filter(LUNDI > date.valid) %>%
      left_join(x.all, by = c('LUNDI', 'ANOP', 'SEMOP')) %>%
      as.data.frame
  })
  
  
  
  # -------------------------------------------------------------------------- #
  # *** 2-6 Models and comparison                                         ----
  # -------------------------------------------------------------------------- #
  
  if(is.null(best))
  {
    # GLM
    printProgress("Step 2/8: GLM Poisson modelling", shiny, {
      
      train.glm <- train[train[,y] >= y_min & train[,y] <= y_max, ] %>%
        mutate(SEMOP = as.factor(SEMOP)) %>% 
        select(-LUNDI)
      if(split[5] != 'AEX'){ train.glm %<>% select(-TOTAL_AEX) }
      
      frml <- as.formula(paste(y, '~', paste(colnames(train.glm[,-3]), collapse = '+')))
      mod.glm <- step(glm(
        formula = frml, 
        family  = family,
        data    = train.glm), 
        trace   = 0
      )
      
      x1 <- colnames(mod.glm$model) %>% setdiff(c('NBVENTP', 'NBSOUMP', 'NPS', 'ANOP', 'SEMOP'))
      print(c('Variables:', paste(x1, collapse = ' - ')))
      
      pred.glm <- predict(
        object  = mod.glm, 
        newdata = mutate(valid, SEMOP = factor(SEMOP)), 
        type    = 'response'
      )
      
    })  

    # ARMA
    printProgress("Step 3/8: TS modelling", shiny, {
      
      start <- c(isoyear(date.start), isoweek(date.start))
      train.ts <- ts(train[,y], start = start, freq = 52)
      train.ts[which(train.ts < y_min)] <- NA
      train.ts[which(train.ts > y_max)] <- NA
      
      mod.arma <- auto.arima(train.ts, allowdrift = T, D = 1, trace = F)
      ord <- mod.arma$arma
      print(c("ARIMA order:", paste(ord[c(1,6,2)], collapse = ' - ')))
      
      pred.arma <- forecast(mod.arma, h = nrow(valid))$mean
      
      # moyenne entre GLM et ARMA
      if(sum(is.na(pred.glm)) + sum(is.na(pred.arma)) == 0){
        pred.glmarma <- (pred.glm + pred.arma) / 2
      } else {
        pred.glmarma <- rep(NA, nrow(valid))
      }
    })  
    
    # ARMAX
    printProgress("Step 4/8: ARIMAX modelling - 1", shiny, {
      
      mod.armax <- try(arimax(
        x        = train.ts, 
        order    = ord[c(1,6,2)],
        seasonal = list(order = ord[c(3,7,4)], period = ord[5]),
        xreg     = train[, x1], 
        method   = 'ML'
      ))
      
      if(class(mod.armax) == 'try-error')
      {
        pred.armax <- rep(NA, nrow(valid))
      } else { 
        pred.armax <- predict(mod.armax, n.ahead = nrow(valid), newxreg = valid[,x1])$pred 
      }
    })

    # STL
    printProgress("Step 5/8: STL modelling", shiny, {
      if(sum(is.na(train.ts)) != 0){ 
        pred.stl <- rep(NA, nrow(valid))
      } else 
      {
        pred.stl <- try(forecast(stl(train.ts, s.window = 52), h = nrow(valid))$mean) 
        if(class(pred.stl) == 'try-error'){ 
          pred.stl <- rep(NA, nrow(valid)) 
        }
        mape.stl <- mean(abs(as.numeric(pred.stl) - valid[,y]) / valid[,y]) 
      }
      
      # moyenne GLM et STL
      if(sum(is.na(pred.glm)) + sum(is.na(pred.stl)) == 0){
        pred.glmstl <- (pred.glm + pred.stl) / 2
      } else {
        pred.glmstl <- rep(NA, nrow(valid))
      }

    })

    # Best model
    printProgress("Step 6/8: Which is the best ?", shiny, {
      
      preds <- cbind(
        ARMA    = pred.arma %>% as.numeric,
        ARMAX   = pred.armax %>% as.numeric, 
        STL     = pred.stl %>% as.numeric,
        GLM     = pred.glm,
        GLMSTL  = pred.glmstl,
        GLMARMA = pred.glmarma
      ) 
      # preds[,"HYBRID"] <- apply(preds, 1, function(x) mean(x, na.rm = TRUE))        
      # autoplot(preds)
      MAPE <- matrix(
        data     = apply(preds, 2, function(x) accuracy(x, valid[,y])[, c('RMSE', 'MAPE')]), 
        ncol     = ncol(preds), 
        byrow    = F,
        dimnames = list(c('RMSE', 'MAPE'), colnames(preds))
      )
      best <- MAPE['MAPE', ] %>% which.min %>% names
      print(MAPE)
    }) 
    
  } # fin du if(is.null(best))
  
  printProgress(paste0("Best is: ", best), shiny, {  })  

  # -------------------------------------------------------------------------- #
  # *** 7. Projections                                                    ----
  # -------------------------------------------------------------------------- #
  
  printProgress("Step 7/8: Projection with best model", shiny, {
    
    train <- serie1 %>%
      group_by(LUNDI, ANOP, SEMOP) %>% #group_by_(.dots = lapply(names(serie1)[-10], as.symbol)) %>% 
      summarise_at(
        vars(one_of(y)), funs(sum)
      ) %>%
      filter(LUNDI >= date.start) %>% 
      mutate(TYPE = 'REAL') %>%
      left_join(x.all, by = c('LUNDI', 'ANOP', 'SEMOP')) %>% 
      as.data.frame
    
    start <- c(isoyear(date.start), isoweek(date.start))
    train.ts <- ts(train[,y], start = start, freq = 52)
    train.ts[which(train.ts < y_min)] <- NA
    train.ts[which(train.ts > y_max)] <- NA
    
    valid <- data.frame(
        LUNDI   = seq(last(train$LUNDI) + 7, as.Date(date.proj), by = 'week')) %>%
      mutate(
        ANOP    = isoyear(LUNDI), 
        SEMOP   = isoweek(LUNDI),
        PRED    = NA, 
        TYPE    = 'FORECAST'
      ) %>%
      rename_own('PRED', y) %>% 
      left_join(x.all, by = c('LUNDI', 'ANOP', 'SEMOP')) %>% 
      as.data.frame
    
    # GLM
    if(grepl('GLM', best))
    {
      
      data <- train[train[,y] >= y_min & train[,y] <= y_max, ] %>% 
        mutate(SEMOP = as.factor(SEMOP)) %>% 
        select(-LUNDI, -TYPE)
      if(split[5] != 'AEX')
        data %<>% select(-TOTAL_AEX)
      
      frml <- as.formula(paste(y, '~', paste(colnames(data)[-3], collapse = '+')))
      mod.final <- step(glm(
        formula = frml, 
        family  = family,
        data    = data), 
        trace   = 0
      )
      x1 <- colnames(mod.final$model) %>% setdiff(c('NBVENTP', 'ANOP', 'SEMOP'))
      print(c('Variables:', paste(x1, collapse = ' - ')))
      
      newdata <- valid %>% mutate(SEMOP = as.factor(SEMOP)) 
      pred.final <- predict(mod.final, newdata, type = 'response') %>% suppressWarnings
      mape <- (100 * abs(resid(mod.final, type = 'response')) / train[,y][train[,y] >= y_min & train[,y] <= y_max]) %>% 
        replace(is.infinite(.), NA) %>% 
        median(na.rm = T)
      
      if(grepl('ARMA', best) | grepl('STL', best)){
        pred.glm <- pred.final
      }
    }

    # ARMA
    if(grepl('ARMA', best)) 
    {
      mod.final <- auto.arima(train.ts, allowdrift = T, D = 1, trace = F)
      pred.final <- forecast(mod.final, h = nrow(valid))$mean
      mape <- (100 * abs(resid(mod.final)) / train[,y]) %>% 
        replace(is.infinite(.), NA) %>% median(na.rm = T)
      
      # GLM-ARMA
      if(grepl('GLM', best)){
        pred.final <- (pred.final + pred.glm) / 2
      }
 
      # ARMAX
      if(grepl('ARMAX', best)) 
      {
        ord <- mod.final$arma
        mod.final <- arimax(
          x        = train.ts, 
          order    = ord[c(1,6,2)], 
          seasonal = list(order = ord[c(3,7,4)], period = ord[5]), 
          xreg     = train[,x1], 
          method   = 'ML'
        )
        
        pred.final <- predict(mod.final, n.ahead = nrow(valid), newxreg = valid[,x1])$pred
        mape <- (100 * abs(resid(mod.final)) / train[,y][train[,y] >= y_min & train[,y] <= y_max]) %>% 
          replace(is.infinite(.), NA) %>% median(na.rm = T)
      }
    }

    # STL
    if(grepl('STL', best))
    { 
      mod.final <- stl(train.ts, s.window = 52)
      pred.final <- forecast(mod.final, h = nrow(valid))$mean
      mape <- 8
      
      # GLM-STL
      if(grepl('GLM', best)) 
      {
        pred.final <- (pred.final + pred.glm) / 2
      }
      
    }
    
  })
 
  # -------------------------------------------------------------------------- #
  # *** 8. Report results                                               ----
  # -------------------------------------------------------------------------- #
  
  printProgress("Step 8/8: Reporting results", shiny, {
    
    final <- valid %>%
      mutate(
        PRED  = round(as.numeric(pred.final), 1),
        MARGE = paste0(round(mape, 1), '%'),
        BEST  = best
      ) %>% 
      rename_own('PRED', y) %>% 
      bind_rows(train) %>%
      mutate(
        SERIE   = serieName,
        VERSION = version
      ) %>% 
      separate(SERIE, c('PROVINCE', 'BANNER', 'GROUP', 'PRODUIT', 'ORIGIN'), sep = '-', remove = F, fill = 'right') %>%
      select(VERSION, SERIE, TYPE, LUNDI, ANOP, SEMOP, one_of(y), MARGE, BEST, PROVINCE, BANNER, GROUP, PRODUIT, ORIGIN) %>%
      mutate_at(
        vars(GROUP, PRODUIT, ORIGIN), funs(replace(., is.na(.), ""))
      ) %>%
      arrange(desc(LUNDI))
    
    return(final)
    
  })
  
}






# ---------------------------------------------------------------------------- #
#
# Old Models                                                              ----
#
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# *** GAMM                                                                ----
# ---------------------------------------------------------------------------- #

# printProgress("Step 7/10: MGCV modelling", shiny, {
#   
#   ctrl <- list(niterEM = 0, msVerbose = T, optimMethod = 'L-BFGS-B')
#   frml <- as.formula(paste(y, '~', paste(c("s(SEMOP, bs = 'cc', k = 52) + s(TIME)", x1), collapse = '+')))
#   mod.gamm <- gamm(
#     formula     = frml, 
#     family      = family,
#     data        = train %>% mutate(TIME = as.numeric(LUNDI) / 100), 
#     control     = ctrl,
#     correlation = corARMA(form = ~1|ANOP, p = 2)
#   )
#   #acf(resid(mod.gamm$lme, type = "normalized"), lag.max = 36, main = "ACF")
#   pred.gamm <- predict(mod.gamm$gam, newdata = valid %>% mutate(TIME = as.numeric(LUNDI) / 100), type = 'response')
# }) 


# if(best == 'GAMM')
# { 
#   if(!is.null(w)) print("poids non pris en compte !!")
#   ctrl <- list(niterEM = 0, msVerbose = T, optimMethod = 'L-BFGS-B')
#   frml <- as.formula(paste(y, '~', paste(c("s(SEMOP, bs = 'cc', k = 52) + s(TIME)", x1), collapse = '+')))
#   mod.final <- gamm(
#     formula     = frml, 
#     data        = train %>% mutate(TIME = as.numeric(LUNDI) / 100), 
#     control     = ctrl,
#     correlation = corARMA(form = ~1|ANOP, p = 2)
#     #weights = w
#   ) 
#   pred.final <- predict(mod.final$gam, newdata = valid %>% mutate(TIME = as.numeric(LUNDI) / 100))
#   mape <- (100 * abs(resid(mod.final$gam)) / train[,y][train[,y] >= y_min & train[,y] <= y_max]) %>% 
#     replace(is.infinite(.), NA) %>% median(na.rm = T)
# }




# ---------------------------------------------------------------------------- #
# *** THIEF                                                               ----
# ---------------------------------------------------------------------------- #

# printProgress("Step 6/10: THIEF modelling", shiny, {
#   
#   if(sum(is.na(train.ts)) != 0){ pred.thief <- rep(NA, nrow(valid))
#   } else {
#     agg <- tsaggregates(train.ts) #autoplot(agg)
#     base <- list(forecast(mod.arma, h = 2 * 52))
#     for(i in 2:length(agg)){ base[[i]] <- forecast(auto.arima(agg[[i]], D = 1), h = 2 * frequency(agg[[i]])) }
#     reconciled <- reconcilethief(base)
#     #plot(reconciled[[1]], main = names(agg)[1], flwd = 1); lines(base[[1]]$mean, col = "red")
#     pred.thief <- data.frame(reconciled[1])$Point.Forecast[1:nrow(valid)]
#     mape.thief <- mean(abs(pred.thief - valid[,y]) / valid[,y])
#   }
#   
# })
# 
# 
# if(best == 'THIEF')
# { 
#   agg <- tsaggregates(train.ts)
#   for(i in 1:length(agg)){ base[[i]] <- forecast(auto.arima(agg[[i]], D = 1), h = 2 * frequency(agg[[i]])) }
#   reconciled <- reconcilethief(base)
#   #plot(reconciled[[1]], main = names(agg)[1], flwd = 1); lines(base[[1]]$mean, col = "red")
#   pred.final <- data.frame(reconciled[1])$Point.Forecast[1:nrow(valid)] 
#   if(exists('mape.thief')){ 
#     mape <- mape.thief #? 
#   } else { 
#     mape <- 8 }
# }




# ---------------------------------------------------------------------------- #
# *** ARIMAX v2                                                           ----
# ---------------------------------------------------------------------------- #

# printProgress("Step 4/10: ARIMAX modelling - 2", shiny, {
# 
#   mod.armax2 <- try(auto.arima(train.ts, allowdrift = T, D = 1, trace = F, xreg = train[,x1]))
# 
#   if(class(mod.armax2)[1] == 'try-error')
#   {
#     pred.armax2 <- rep(NA, nrow(valid))
#   } else {
#     pred.armax2 <- forecast(mod.armax2, h = nrow(valid), xreg = valid[,x1])$mean
#   }
# })
# 
# 
# if(best == 'ARMAX2') 
# {
#   mod.final <- auto.arima(
#     train.ts, 
#     allowdrift = T, 
#     D = 1, 
#     trace = F, 
#     xreg = train[,x1]
#   )
#   pred.final <- forecast(mod.final, h = nrow(valid), xreg = valid[,x1])$mean
#   mape <- try((100 * abs(resid(mod.final)) / train[,y][train[,y] >= y_min & train[,y] <= y_max]) %>% 
#                 replace(is.infinite(.), NA) %>% median(na.rm = T))
#   if(class(mape) == 'try-error'){ mape <- 0 }
# }





