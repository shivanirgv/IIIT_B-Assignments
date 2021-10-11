func_limit_outliers <- function(x) 
{
  lower_limit <- quantile(x,0.25,na.rm = T) - 1.5*IQR(x,na.rm = T)
  upper_limit <- quantile(x,0.75,na.rm = T) + 1.5*IQR(x,na.rm = T)
  x[which(x<=lower_limit)] <- lower_limit
  x[which(x>=upper_limit)] <- upper_limit
  x
}

##adding a function to add the moving average for the variable passed as the 
#argument. This method returns the data frame with moving average value
addMovingAverage <- function(ma_var,df){
    ma_2 = frollmean(df[, ma_var], 2)
    ma_3 = frollmean(df[, ma_var], 3)
    ma_4 = frollmean(df[, ma_var], 4)
    temp <- as.data.frame(cbind(ma_2,ma_3,ma_4))
    names(temp) <- c(paste(ma_var,'ma_2', sep="_"),paste(ma_var,'ma_3', sep="_"),paste(ma_var,'ma_4', sep="_"))
    return(temp)
}

calcIncrInMAVariable <- function(ma_var,df){
  ma_df = addMovingAverage(ma_var,df)
  ma2_df_var_name = paste(ma_var,'ma_2', sep="_")
  ma3_df_var_name = paste(ma_var,'ma_3', sep="_")
  ma4_df_var_name = paste(ma_var,'ma_4', sep="_")
  ma2 = (df[,ma_var] - ma_df[,ma2_df_var_name])/ma_df[,ma2_df_var_name]
  ma3 = (df[,ma_var] - ma_df[,ma3_df_var_name])/ma_df[,ma3_df_var_name]
  ma4 = (df[,ma_var] - ma_df[,ma4_df_var_name])/ma_df[,ma4_df_var_name]
  temp <- as.data.frame(cbind(ma2,ma3,ma4))
  names(temp) <- c(paste('inc',ma_var,'MA2', sep="_"),paste('inc',ma_var,'MA3', sep="_"),paste('inc',ma_var,'MA4', sep="_"))
  return(temp)
}

elasticity <- function(var,df,final_model,mm){
  elax1 <-ifelse(mm==1,as.numeric(final_model$coefficients[var]),as.numeric(final_model$coefficients[var]*mean(df[,var])/mean(df$total_gmv)))
  return(elax1)
} 

calcElasticity <- function(final_model,df,mm){
  elasticity <- vector(mode="numeric")
  variable <- vector(mode="character")
  for(i in 2:length(final_model$coefficients)){
    variable <- c(variable,as.character(names(final_model$coefficients)[i]))
    elasticity <-c(elasticity,as.numeric(elasticity(names(final_model$coefficients)[i],df,final_model,mm)))
  }
  elastic_df <- data.frame(cbind(variable,elasticity))
  elastic_df$variable <- as.character(elastic_df$variable)
  elastic_df$elasticity <- as.numeric(as.character(elastic_df$elasticity))
  elastic_df$direction <-  ifelse(as.numeric(elastic_df$elasticity) > 0, "Positive", "Negative")
  return(elastic_df)
}

plotElasticity <- function(elasticity_df,title){
  plot <- ggplot(data=elasticity_df, aes(x=reorder(variable,elasticity),y=elasticity)) +
    geom_bar(position="dodge",stat="identity",fill="blue") + 
    coord_flip() +
    ggtitle(title) +xlab("Variables")+ ylab("Elasticity")+
    geom_text(aes(label=round(elasticity,2)), size=4)
  return(plot)
}

plotElasticity_grid <- function(plot1,plot2,plot3,plot4,plot5){
  p <- plot_grid(plot1,plot2,plot3,plot4,plot5, align = "v",ncol = 2)
  plot_grid(p, ncol=1, rel_heights=c(0.1, 1))
}
# geom_text(aes(label=formatC(elasticity,format="e",digits = 2)), size=4)