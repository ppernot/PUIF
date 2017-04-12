case='Ar'
model_tag = 'Margin'

fit_tag = paste0("fit",model_tag)
load(paste0('simulation/',case,'/',fit_tag,'.rda'))

X=extract(fit,pars=c('u_eps','u_sig','rho','lp__'))
Y=cbind(X$u_eps,X$u_sig,X$rho,X$lp__)
colnames(Y)=c('u_eps','u_sig','rho','log_post.')

col2tr = function(col,alpha)
  rgb(t(col2rgb(col)),alpha=alpha,maxColorValue=255)
green_tr  =col2tr("darkgreen"  ,10)

dir_out = paste0('simulation/',case,'/figures')
png(file=paste0(dir_out,'/post_sample_',model_tag,'.png'),
    width=1200,height=1200)

panel.hist <- function(x,...) {
  usr <- par("usr"); on.exit(par(usr))     
  par(usr = c(usr[1:2], 0, 1.5))     
  h <- hist(x, plot = FALSE)     
  breaks <- h$breaks;
  nB <- length(breaks)   
  y <- h$counts; y <- y/max(y)
  # grid(col='brown',ny=0)
  rect(breaks[-nB],0,breaks[-1],y,col="orange")
}  
pairs(Y[sample.int(nrow(Y),5000),], diag.panel = panel.hist,
      gap=0, col=green_tr, pch=16,
      cex=3, cex.axis= 3, cex.labels=4)
dev.off()
