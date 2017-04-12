# Graphics params
pch      = 19
pch.cex  = 1
pch.col  = 'darkgreen'
pch.lwd  = 2
errb.col = pch.col
errb.lwd = 2
line.lwd = 2
line.col = 'orange'
box.lwd  = 2
grid.lwd = 1
grid.col = 'gray30'
blue_tr=rgb(unlist(t(col2rgb("blue"))),
            alpha=60,maxColorValue = 255)
cexPlot = 3.5

method = 6
basis  = 1

# 1- Get data save from run of 1_scrap_CCCBDB ####
table=read.table(file=paste0('data/freq_',method,'_',basis,'.csv'),
             sep=',',header=FALSE, allowEscapes=FALSE, quote="\"",
             fileEncoding='utf8', stringsAsFactors = FALSE)
calc = table[,2]
mesu = table[,3]

# 2- Linear regression and stats ####
reg  = lm(mesu~0+calc)
resid = residuals(reg)
sse   = sum(resid^2)
us    = sqrt(sse/sum(calc^2))
lims = range(c(calc,mesu)) 

# 3- Generate figures ####
dir.create('figures',showWarnings = FALSE)

png(file='figures/scale_factors0.png',width=800,height=800)
par(mfrow=c(1,1),mar=c(3,3,1.6,.5),mgp=c(2,.75,0),
    tcl=-0.5, cex=cexPlot)
plot(calc,mesu,
     pch=pch,cex=pch.cex/5,col=pch.col,lwd=pch.lwd,
     xaxs='i',yaxs='i',      
     xlab='Calculated frequency / cm-1',
     ylab='Reference frequency / cm-1',
     xlim=lims,ylim=lims)
abline(reg,lty=1,lwd=line.lwd,col=2)
abline(a=0,b=1,lty=2,lwd=line.lwd,col=1)
grid(col=grid.col,lwd=grid.lwd)
box(lwd=box.lwd)
dev.off()

png(file='figures/scale_factors.png',width=1600,height=800)
par(mfrow=c(1,2),mar=c(3,3,1.6,.5),mgp=c(2,.75,0),
    tcl=-0.5, cex=cexPlot)

# Variance Inflation
plot(calc,resid,
     pch=pch,cex=pch.cex/5,col=pch.col,lwd=pch.lwd,
     xaxs='i',yaxs='i',      
     xlab='Calculated frequency / cm-1',
     ylab='Residual / cm-1',
     xlim=lims,ylim=c(-200,300),
     main='Variance Inflation')
abline(h=0,lty=2,lwd=line.lwd)

dC = (exp(2*us)-1)*calc
cols = ifelse((resid+dC)*(resid-dC)<=0,'green','red')
sel = cols =='green'
good = sum(sel)
sel = cols =='red'
points(calc[sel],resid[sel],     
       pch=pch,cex=pch.cex/4,col=cols[sel],lwd=pch.lwd)
abline(a=0,b=-2*us,col=line.col,lty=1,lwd=line.lwd)
abline(a=0,b= 2*us,col=line.col,lty=1,lwd=line.lwd)

grid(col=grid.col,lwd=grid.lwd)
box(lwd=box.lwd)
legend('topleft',
       legend=c(paste0('Accept: ',round(100*good/length(sel),digits=0),' %'),'Reject'),
       cex=0.7,
       pch=pch, pt.cex = pch.cex/4,
       col=c(pch.col,'red'))
       
       
# Dispersion correction
rmse = (sse/length(calc))^0.5
dC = 2*rmse
cols = ifelse((resid+dC)*(resid-dC)<=0,'green','red')
sel = cols =='green'
good = sum(sel)
sel = cols =='red'
plot(calc,resid,
     pch=pch,cex=pch.cex/4,col=pch.col,lwd=pch.lwd,
     xaxs='i',yaxs='i',      
     xlab='Calculated frequency / cm-1',
     ylab='Residual / cm-1',
     xlim=lims,ylim=c(-200,250),
     main='Dispersion Correction')
abline(h=0,lty=2,lwd=line.lwd)
points(calc[sel],resid[sel],     
       pch=pch,cex=pch.cex/4,col=cols[sel],lwd=pch.lwd)
abline(h=c(-1,1)*dC,col=line.col,lty=1,lwd=line.lwd)
grid(col=grid.col,lwd=grid.lwd)
box(lwd=box.lwd)
legend('topleft',
       legend=c(paste0('Accept: ',round(100*good/length(sel),digits=0),' %'),'Reject'),
       cex=0.7,
       pch=pch, pt.cex = pch.cex/4,
       col=c(pch.col,'red'))

dev.off()
