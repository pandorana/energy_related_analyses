getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

getInfo <- function(vector) {
  result <- tribble(
    ~variable, ~value,
    "mean", mean(vector),
    "SD", sd(vector),
    "median", median(vector),
    "Q1", quantile(vector, 1/4),
    "Q3", quantile(vector, 3/4),
    "mode", getMode(vector),
    "min", min(vector),
    "max", max(vector)
  )
  return(result)
}

plotResid <- function(mod, x, xName = NULL, flip = FALSE) {
  p <- augment(mod) %>% ggplot(aes(x, .resid)) + geom_ref_line(h=0) + geom_point() + 
       geom_smooth(aes(color="red"), se=FALSE) + theme(legend.position = "none")
  if(flip)
    p + xlab(ifelse(is.null(xName), "x", xName)) + ylab("Residual") + coord_flip()
  else
    p + xlab(ifelse(is.null(xName), "x", xName)) + ylab("Residual")
}

plotFit2True <- function(mod, func=NULL) {
    augment(mod) %>% ggplot(aes(.[1], .fitted)) + geom_point() + geom_smooth(se=FALSE)
}

plotMod <- function(mod, des=NULL) {
  if (!is.null(des))
    pdf(paste("graphs/", des, ".pdf", sep = ""))
  par(mfrow = c(2,2))
  plot(mod)
  par(mfrow = c(1,1))
  if(!is.null(des))
    dev.off()
}

writeNum <- function(num, dec_p=2) {
  format(round(num, dec_p), nsmall = 2)
}

writePValue <- function(p) {
  ifelse(writeNum(p,3) %>% as.numeric() < 0.001, 
         "< .001", 
         writeNum(p,3))
}

writePercent <- function(perc) {
  format(round(perc * 100, 2), nsmall = 2)
}

writeCategories <- function(categories) {
  categories[1] <- categories[[1]] %>% as.factor()
  paste(apply(categories, 1, function(category) paste(category[[1]], " (",trimws(category[[2]]), ")", sep="")), 
        collapse = ", ")
}

writeContinuous <- function(variable) {
  paste("M = ", writeNum(mean(variable)), ", SD = ", writeNum(sd(variable)), sep = "")
}

getLassoResult <- function(fit, X, se = 0) {
  pos <- min(which(fit$lldiff <= min(fit$lldiff) + se * sd(fit$lldiff)))
  if (all(fit$fit$beta[,pos] == 0))
    fit <- fit$fit$beta[,pos+1]
  else
    fit <- fit$fit$beta[,pos]
  fit %>% as.tibble() %>%
    rename(coefficient = value) %>% 
    mutate(variable = attr(X, which = "dimnames")[[2]]) %>% 
    select(variable,coefficient)
}

hasSignificance <- function(mod) {
  if (class(mod)[1] == "lm")
    ifelse(glance(mod)$p.value <= 0.05, "\b", ", with no statistic significance,")
  else if (class(mod)[1] == "glm")
    ifelse(analysis_Logit(mod)[[2]] <= 0.05, "\b", ", with no statistic significance,")
}

analysis_Logit <- function(mod) {
  mod.null <- glm(as.formula(paste(mod$formula %>% as.character() %>% .[2], " ~ 1", sep = "")), 
                  family = binomial, mod$model)
  aov <- anova(mod.null, mod, test = "Chisq")
  tribble(
    ~pseudo.r.squared, ~p.value,
    1 - glance(mod)$logLik/glance(mod.null)$logLik, aov$`Pr(>Chi)`[2])
}

pOfDifference <- function(mod1, mod2) {
  if(class(mod1)[1] == "lm")
    anova(mod1, mod2)$`Pr(>F)`[2] %>% writePValue()
  else if (class(mod1)[1] == "glm")
    with(anova(mod1, mod2),pchisq(Deviance,Df,lower.tail=FALSE)[2]) %>% writePValue()
}

fratio_index <- function(cluster) {
  glance(cluster)[["tot.withinss"]] / glance(cluster)[["betweenss"]] * nrow(tidy(cluster))
}

n_kmeans <- function(x, max.centers, min.centers = 2) {
  kclusts <- data.frame(k = min.centers:max.centers) %>% 
              group_by(k) %>% 
              do(kclust=kmeans(x, .$k)) %>%
              mutate(fratio = fratio_index(kclust))
}

plot_FRatio_4_nclusters <- function(clusters) {
  for (i in 1:(nrow(clusters)-1)) {
    if (clusters$fratio[i] < clusters$fratio[i+1])
      return (clusters %>% ggplot(aes(k, fratio)) + geom_line() + 
        geom_point(data = clusters[i,], shape = "x", color = "red", size = 10))
  }
  clusters %>% ggplot(aes(k, fratio)) + geom_line() + 
    geom_point(data = clusters[nrow(clusters),], shape = "x", color = "red", size = 10)
}

best_cluster <- function(clusters) {
  for (i in 1:(nrow(clusters)-1)) {
    if (clusters$fratio[i] < clusters$fratio[i+1])
      return(clusters$kclust[[i]])
  }
  return(clusters$kclust[[nrow(clusters)]])
}

tidy.beta <- function(mod) {
  summary(lm.beta(mod))$coefficients %>% as.tibble() %>% rownames_to_column() %>% .[c(2:nrow(.),1),2:ncol(.)]
}

predictor_add_stars <- function(predictor, p.value) {
  predictor = ifelse(!is.na(p.value),
              ifelse(p.value < 0.001, paste(predictor, "***", sep=""),
              ifelse(p.value < 0.01, paste(predictor, "**", sep=""),
              ifelse(p.value < 0.05, paste(predictor, "*", sep=""), 
                                     predictor))), 
                     predictor)
}

reorder_cluster <- function(clusters, by) {
  rela <- function(x) {
    x <- 1:length(x) %>% as.character()
  }
  clusters$cluster %>% as.factor() %>% fct_reorder(by, mean) %>% fct_relabel(rela)
}
