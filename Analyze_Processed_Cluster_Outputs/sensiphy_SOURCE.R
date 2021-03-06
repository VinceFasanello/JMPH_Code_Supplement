#tree_phylm only treats a single predictor
#my_tree_phylm yields handles multiple predictors

# run as, but replace n.tree with the #trees you want this to run over
# sensimod<-my_tree_phylm(formula=I(log(cost)) ~ PC1.tasbreadth + PC2.mtnmass +  PC3.boundarylength + PC4.dispersalability,
#               data = mydata,phy=trees,track = TRUE,add_median_residuals=T,n.tree = 2)
# summary(sensimod)
# my_sensimod(sensimod)


my_tree_phylm_new<-function (formula, data, phy, n.tree = 2, model = "lambda", nb, basecols, moran_p = 0.05,
                             track = TRUE, add_median_residuals=T,...) {
  if (!inherits(formula, "formula")) 
    stop("formula must be class 'formula'")
  if (!inherits(data, "data.frame")) 
    stop("data must be class 'data.frame'")
  if (!inherits(phy, "multiPhylo")) 
    stop("phy must be class 'multiPhylo'")
  if (length(phy) < n.tree) 
    stop("'n.tree' must be smaller (or equal) than the number of trees in the 'multiPhylo' object")
  if ((model == "trend") && (sum(ape::is.ultrametric(phy)) > 
                             1)) 
    stop("Trend is unidentifiable for ultrametric trees., see ?phylolm for details")
  else datphy <- match_dataphy(formula, data, phy, ...)
  full.data <- datphy[[1]]
  phy <- datphy[[2]]
  #trees <- sample(length(phy), n.tree, replace = F) # comment this out and pass it as function argument whichtrees
  
  
  #formula_args<- unlist(strsplit(strsplit(as.character(formula), split=" ~ ", fixed = T)[[3]], split=" + ", fixed = T)) # replace this with model coefficients because of categorical preds
  mod = try(phylolm::phylolm(formula, data = full.data,  model = model, phy = phy[[1]]), FALSE)
  names(coef(mod))->formula_args
  formula_args<-formula_args[!formula_args%in%"(Intercept)"] # to follow JB's code
  
  p<-expand.grid(c("(Intercept)", formula_args), c("", ".se", ".pval"))
  sensi.estimates.cols<-c("n.tree",paste(p$Var1, p$Var2, sep=""),"aic", "optpar")
  sensi.estimates <- data.frame(matrix(rep(NA, length(sensi.estimates.cols)), nrow = 1))
  sensi.estimates<-sensi.estimates[-1,]
  names(sensi.estimates)<-sensi.estimates.cols
  counter = 1
  errors <- NULL
  c.data <- list()
  residuals<-data.frame("Species"=rownames(full.data))
  if (track == TRUE) 
    pb <- utils::txtProgressBar(min = 0, max = n.tree, style = 3)
  
  
  for (j in seq(from=1,to=n.tree)) {
    full.data <- full.data[phy[[j]]$tip.label, ]
    
    # model
    mod = try(phylolm::phylolm(formula, data = full.data, model = model, phy = phy[[j]]), FALSE)
    if (isTRUE(class(mod) == "try-error")) {
      error <- j
      names(error) <- rownames(c.data$full.data)[j]
      errors <- c(errors, error)
      next
    }
    else {
      
      
      # HERE WE SHOULD ADD SPATIAL TESTING
      # ie check Moran's I
      mor <- moran.test(residuals(mod), nb2listw(nb, zero.policy = TRUE), zero.policy = TRUE) 
      # if there is no evidence for AC --> continue below
      # if there is - run sarcol() and re-run mod by adding fitted(sarcol) in the model
      if (mor$p.value <= moran_p){
        sarcol <- SpatialFiltering(formula = formula, data = full.data, nb=nb, style="W", ExactEV = TRUE, zero.policy = TRUE)
        full.data[,c((basecols+1):(basecols+1+ dim(fitted(sarcol))[2]-1))]<-fitted(sarcol)
        colnames(full.data)[(basecols+1):ncol(full.data)] <- c(paste0("V", 1:length((basecols+1):ncol(full.data))))
        nspace_terms <- ncol(full.data) - basecols
        if(nspace_terms == 0){formula_space <- formula; vadded <- 0}
        if(nspace_terms == 1){formula_space <- merge(formula, ~V1); vadded <- 1}
        if(nspace_terms == 2){formula_space <- merge(formula, ~V1+V2); vadded <- 2}
        if(nspace_terms == 3){formula_space <- merge(formula, ~V1+V2+V3); vadded <- 3}
        if(nspace_terms == 4){formula_space <- merge(formula, ~V1+V2+V3+V4); vadded <- 4}
        if(nspace_terms == 5){formula_space <- merge(formula, ~V1+V2+V3+V4+V5); vadded <- 5}
        if(nspace_terms == 6){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6); vadded <- 6}
        if(nspace_terms == 7){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7); vadded <- 7}
        if(nspace_terms == 8){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8); vadded <- 8}
        if(nspace_terms == 9){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9); vadded <- 9}
        if(nspace_terms == 10){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10); vadded <- 10}
        if(nspace_terms == 11){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11); vadded <- 11}
        if(nspace_terms == 12){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12); vadded <- 12}
        if(nspace_terms == 13){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13); vadded <- 13}
        if(nspace_terms == 14){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14); vadded <- 14}
        if(nspace_terms == 15){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15); vadded <- 15}
        if(nspace_terms >= 16){formula_space <- merge(formula, ~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16); vadded <- 16}
        print(paste0("spatial filtering applied to tree#: ", j, ": ", nspace_terms, " spatial terms added"))
        # formula2 = merge(formula)
        mod = try(phylolm::phylolm(formula_space, data = full.data, model = model, phy = phy[[j]]), FALSE)
        full.data <- full.data[,1:basecols]
        rm(sarcol, nspace_terms, formula_space)
      } else {
        vadded <- 0
      }
      # I would then prune the dataframe below to exclude the spatial vectors, because these might be different depending on the tree --> and you want to focus anyway only on the actual predictors (PCs)
      
      Beta<-data.frame(phylolm::summary.phylolm(mod)$coefficients[,c(1,2,4)])
      Beta<-Beta[1:(nrow(Beta) - vadded),]
      Beta<-cbind(rownames(Beta), Beta)
      names(Beta)<-c("var", "_", ".se", ".pval")
      rownames(Beta)<-NULL
      Beta_df<-gather(Beta, "_", ".se", ".pval", key="p", value="number")
      Beta_df$p<-gsub(Beta_df$p, pattern = "_", replacement = "")
      Beta_df$pp<-paste(Beta_df$var, Beta_df$p, sep="")
      #Beta_df<-Beta_df %>% select('pp', 'number') %>% t %>% data.frame # this for some reason errors, replace with below 
      newdf<-t(as.matrix(data.frame("pp"=Beta_df$pp, "number" = Beta_df$number)))
      newdf<-as.data.frame(newdf)
      Beta_df<-newdf
      
      cnames<-as.character(as.matrix(Beta_df[1,]))
      Beta_df<-Beta_df[2,]
      colnames(Beta_df)<-cnames
      #Beta_df<-as.numeric(as.matrix(Beta_df[2,]))
      #names(Beta_df)<-Beta_df[1,]
      #Beta_df<-Beta_df[2,] %>% mutate_all(as.numeric)
      
      
      # intercept <- phylolm::summary.phylolm(mod)$coefficients[[1,1]]
      # se.intercept <- phylolm::summary.phylolm(mod)$coefficients[[1,2]]
      # estimate <- phylolm::summary.phylolm(mod)$coefficients[[2, 1]]
      # se.estimate <- phylolm::summary.phylolm(mod)$coefficients[[2, 2]]
      # pval.intercept <- phylolm::summary.phylolm(mod)$coefficients[[1, 4]]
      # pval.estimate <- phylolm::summary.phylolm(mod)$coefficients[[2, 4]]
      aic.mod <- mod$aic
      n <- mod$n
      d <- mod$d
      if (model == "BM") {
        optpar <- NA
      }
      if (model != "BM") {
        optpar <- mod$optpar
      }
      if (track == TRUE) 
        utils::setTxtProgressBar(pb, counter)
      
      # estim.simu <- Beta_df %>% mutate(j=j, aic.mod=aic.mod, optpar=optpar) %>%
      #   select(c(j, names(Beta_df), aic.mod, optpar))
      # for some reason this errors - replace with below
      estim.simu<-as.data.frame(t(as.matrix(c(j,as.numeric(as.matrix(Beta_df[1,])),aic.mod,optpar))))
      colnames(estim.simu)<c(j, colnames(Beta_df),"aic.mod","optpar")
      
      # data.frame(j, 
      #                        intercept, se.intercept, 
      #                        pval.intercept, estimate, se.estimate, pval.estimate, 
      #                        aic.mod, optpar, 
      #                        stringsAsFactors = F)
      
      #sensi.estimates[counter, ] <- estim.simu
      sensi.estimates[counter, ] <- as.numeric(as.matrix(estim.simu)) # replace with this
      
    }
    if(add_median_residuals){
      these_residuals<-residuals(mod)[residuals$Species]
      residuals<-cbind(residuals, "res"=these_residuals)
      colnames(residuals)<-c("Species", paste("iteration", 1:counter, sep = "_"))
    }
    counter = counter + 1
  }
  if (track == TRUE) 
    on.exit(close(pb))
  statresults <- data.frame(median=apply(sensi.estimates, 2, median),
                            min=apply(sensi.estimates, 2, min),
                            max=apply(sensi.estimates, 2, max),
                            sd_tree=apply(sensi.estimates, 2, stats::sd))
  # data.frame(min = apply(sensi.estimates, 2, min), 
  #                         max = apply(sensi.estimates, 2, max), 
  #                         mean = apply(sensi.estimates, 2, mean), 
  #                         sd_tree = apply(sensi.estimates, 2, stats::sd))[-1,]
  # statresults$CI_low <- statresults$mean - qt(0.975, df = n.tree - 
  #                                               1) * statresults$sd_tree/sqrt(n.tree)
  # statresults$CI_high <- statresults$mean + qt(0.975, df = n.tree - 
  #                                                1) * statresults$sd_tree/sqrt(n.tree)
  statresults$row<-row.names(statresults)
  statresults<-statresults[order(statresults$row),]
  statresults$row<-NULL
  
  if(add_median_residuals){
    residuals$median_residuals<-apply(residuals[,2:ncol(residuals)], 1, median)
    #residuals<-residuals %>% select(c("Species", "median_residuals"))     # this errors for some reason, replaced with below
    residuals<-residuals[,colnames(residuals)%in%c("Species", "median_residuals")]
  }
  
  res <- list(call = match.call(), formula = formula, #data = full.data, 
              sensi.estimates = sensi.estimates, N.obs = n, 
              stats = round(statresults, digits = 3),
              #           [
              #c(1:6)
              #              , c(3, 5, 6)], 
              #                          digits = 3), 
              all.stats = statresults)
  if(add_median_residuals){
    res$residuals<-residuals
  }
  class(res) <- "sensiTree"
  return(res)
}


## function that summarizes the sensimode output in a nicer way (I thought)
my_sensimod<-function(sensimod_RNB){
  coefs<-sensimod_RNB$all.stats
  
  ntreestats<-coefs[rownames(coefs)%in%"n.tree",]
  aicstats<-coefs[rownames(coefs)%in%"aic",]
  optparstats<-coefs[rownames(coefs)%in%"optpar",]
  listrest<-list(ntreestats,aicstats,optparstats)
  
  rownames(coefs)->allvars
  allvars<-allvars[!allvars%in%c("n.tree","aic","optpar")]
  vars<-allvars[grep(".pval",allvars)]
  vars<-gsub(".pval","", vars)
  
  # build coefs
  summodel<-data.frame("Var"=vars,"Estimate"=0,"StdErr"=0,"st_tree"=0,"pval"=0,"pvalstar"="", "%significant"=0)
  summodel$Var<-as.character(summodel$Var)
  summodel$pvalstar<-as.character(summodel$pvalstar)
  
  for(j in 1:length(summodel[,1])){
    varj<-summodel$Var[j]
    estimj<-coefs[rownames(coefs)%in%varj,]$median
    streej<-coefs[rownames(coefs)%in%varj,]$sd_tree
    sej<-coefs[rownames(coefs)%in%paste0(varj,".se"),]$median
    pvalj<-coefs[rownames(coefs)%in%paste0(varj,".pval"),]$median
    pvalstarj<-ifelse(pvalj<0.001,"***",ifelse(pvalj>=0.001 & pvalj<0.01, "**", ifelse (pvalj>0.01 & pvalj<0.05,"*","")))
    
    psign<-sum(sensimod_RNB$sensi.estimates[,colnames(sensimod_RNB$sensi.estimates)%in%paste0(varj,".pval")]<0.05) / length(sensimod_RNB$sensi.estimates[,colnames(sensimod_RNB$sensi.estimates)%in%paste0(varj,".pval")])
    summodel[j,]<-c(varj,estimj,sej,streej,pvalj,pvalstarj,psign)
  }
  
  toret<-list(summodel, listrest)
  names(toret)<-c("summodel","listrest")
  return(toret) 
  
}
