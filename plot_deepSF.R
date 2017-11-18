setwd("/mnt/data/workspace/cpp-spacefortress")

require(data.table)
require(ggplot2)
require(reshape2)
require(stringr)

thresholds = data.table(
  variable = c("points","destroyedFortresses","totalShots"),
  threshold = c(3748, 49, 49*12),
  name = c("max_points", "max_kills","min_shots")
)

while (TRUE) {
  try({
    d = rbindlist(lapply(list.files(pattern="*.log"),function(x) {
      .d = fread(x)
      tokens = str_split(str_sub(x,end=-5),"_")[[1]]
      .d[,model:=paste(tokens[4],tokens[11],sep="_")]
      }),fill=TRUE)
    d[,model:=as.factor(model)]
    d[,c("missedShots","totalLefts","totalRights"):=NULL]
    #d[,c("action0_p","action1_p","action2_p"):=NULL]
    d[,c("mean_eps","mean_tau"):=NULL]
    dd = melt(d[episode>0],id.vars=c("epoch","episode","training","model"))[!is.nan(value)]
    dd[,epoch:=ceiling(episode/10)]
    p = ggplot(dd[epoch>0,.(mean_value=mean(value),sd_value=sd(value)),by=c("model","variable","epoch")]) + 
      geom_line(aes(x=as.numeric(epoch),y=mean_value,color=model),alpha=.8) + 
      geom_ribbon(aes(x=as.numeric(epoch),ymin=mean_value-sd_value,ymax=mean_value+sd_value,fill=model),alpha=.4) + 
      #geom_hline(aes(yintercept=threshold,color=name),data=thresholds) +
      facet_wrap(~variable,scales="free_y",ncol=4) +
      theme(legend.position="right") +
      guides(color=guide_legend(ncol=1)) +
      scale_color_brewer("Model",palette="Spectral") +
      scale_fill_brewer("Model",palette="Spectral") +
      #scale_color_manual(values=RColorBrewer::brewer.pal(6,"Paired")[c(1,2,5,6)]) +
      theme_dark()
    print(p)
  })
  Sys.sleep(120)
}