# Load (install) necessary packages
if(!require(XML))
  install.packages("XML",dependencies=TRUE)
library(XML)
if(!require(outliers))
  install.packages("outliers",dependencies=TRUE)
library(outliers)

# Graphics params

pch      = 1
pch.cex  = 1
pch.col  = 'blue'
pch.lwd  = 2
errb.col = pch.col
errb.lwd = 2
line.lwd = 2
line.col = 'red'
box.lwd  = 2
grid.lwd = 1
grid.col = 'gray30'
blue_tr=rgb(unlist(t(col2rgb("blue"))),
            alpha=60,maxColorValue = 255)
# cexPlot = 3.5 # for 1200x1200 file output
cexPlot = 1.5 # for screen aouput

# Select method/basis set combination
method = 6 # CCD
basis  = 1 # 6-31G*


# 1- Get the table on the web site ####
# - which : the data of interest are in the 5th table
#           in the HTML page (trial and error...)
# - colClasses: providing the type of the columns helps a lot

u = paste0("http://cccbdb.nist.gov/vibscale2.asp?",
           "method=",method,
           "&basis=",basis)
table = readHTMLTable(u,
                      header=TRUE,
                      which=5,
                      stringsAsFactors=FALSE,
                      encoding='utf8',
                      colClasses=c('character','character',
                                   'numeric','character',
                                   'numeric','numeric',
                                   'numeric','numeric',
                                   'numeric')
                      )

# Get rid of molecules names, symmetries and last 3 columns
table=table[,-c(2,3,4,7,8,9)]

# Fill missing info in col. 1
string = intToUtf8(160)
for (i in 2:nrow(table))
  if(table[i,1] == string) table[i,1]=table[i-1,1]

# Remove lines with absent data
sel = !is.na(table[,2]) & !is.na(table[,3])
table = table[sel,]

# Detect and Remove outliers
calc = table[,2] 
mesu = table[,3] 
reg1 = lm(mesu~0+calc)

lims = range(c(calc,mesu)) 
par(mfrow=c(1,1),mar=c(3,3,1.6,.5),mgp=c(2,.75,0),
    tcl=-0.5, cex=cexPlot)
plot(calc,mesu,
     pch=pch,cex=pch.cex/5,col=pch.col,lwd=pch.lwd,
     xaxs='i',yaxs='i',      
     xlab='Calculated frequency / cm-1',
     ylab='Reference frequency / cm-1',
     xlim=lims, ylim=lims)
abline(a=0,b=1,lty=2,lwd=line.lwd)
abline(reg1,col=line.col,lty=2,lwd=line.lwd)
names = table[,1] 
resid = residuals(reg1)
for (i in 1:20) {     
  iout = which(outlier(resid,logical=TRUE))     
  points(calc[iout],mesu[iout],col=2,cex=0.8)
  text(calc[iout],mesu[iout],names[iout],pos=4,offset=0.2,cex=0.5)    
  resid = resid[-iout]     
  calc  = calc[-iout]     
  mesu  = mesu[-iout]     
  names = names[-iout] 
}
grid(col=grid.col,lwd=grid.lwd)
box(lwd=box.lwd)

# 2- Identify problematic systems on graph and list them here ####
probList=c('CH3OCH2CN','C8H8','BH2','CN')

# Get rid of outliers and check again
# If necessary, update the list above...
newTable=subset(table, !(table[,1] %in% probList))
data = newTable[,2:3] 
calc = data[,1] 
mesu = data[,2] 
reg2 = lm(mesu~0+calc)
nMol = length(unique(newTable[,1]))

plot(calc,mesu,
     pch=pch,cex=pch.cex/5,col=pch.col,lwd=pch.lwd,
     xaxs='i',yaxs='i',      
     xlab='Calculated frequency / cm-1',
     ylab='Reference frequency / cm-1',
     xlim=lims, ylim=lims)
abline(a=0,b=1,lty=2,lwd=line.lwd)
abline(reg2,col=line.col,lty=2,lwd=line.lwd)
names = newTable[,1] 
resid = residuals(reg2)
for (i in 1:15) {     
  iout = which(outlier(resid,logical=TRUE))     
  points(calc[iout],mesu[iout],col=2,cex=0.8)
  text(calc[iout],mesu[iout],names[iout],adj=1.5,cex=0.5)    
  resid = resid[-iout]     
  calc  = calc[-iout]     
  mesu  = mesu[-iout]     
  names = names[-iout] 
}
grid(col=grid.col,lwd=grid.lwd)
box(lwd=box.lwd)

# 3- Save data in csv file ####
write.table(newTable,
            file=paste0('data/freq_',method,'_',basis,'.csv'),
            col.names = FALSE,
            row.names = FALSE,
            sep=',')