dir.create(path=paste0('simulation/',case), 
           showWarnings = FALSE)
dir.create(path=paste0('simulation/',case,'/tables'), 
           showWarnings = FALSE)
dir.create(path=paste0('simulation/',case,'/figures'), 
           showWarnings = FALSE)
dir.create(path=paste0('simulation/',case,'/diagnostics'), 
           showWarnings = FALSE)

# Generate data ####
set.seed(127)
if(substr(case,1,2)=='SD') {
  data = gen_synth_data(shift=shift)
} else {
  data = get_ref_data()
}

sink(paste0('simulation/',case,'/data.tex'))
knitr::kable(cbind(data$tag,data$x,data$y,signif(data$uy,2)), format='latex')
sink()

# Run simulations ####
for( model_tag in list_meth ) {
  simul_file = paste0('simulation/',case,'/fit',model_tag,'.rda')
  if(!file.exists(simul_file)) {
    stan_model = 
      paste0(
        readLines(con=paste0('code/models/',model_tag,'.stan')),
        collapse='\n'
      )
    source(paste0('code/models/',model_tag,'.R'))
    eltim = get_elapsed_time(fit)
    
    save(fit, data, parOpt, eltim, file=simul_file)

    # Diagnostic outputs
    print(fit, pars=c(parOpt,'br','lp__'))
    
    sink(file=paste0('simulation/',case,'/diagnostics/summary_',model_tag,'.txt'))
    print(fit, pars=c(parOpt,'br','lp__'))
    cat('\n')
    cat('*** Elapsed Time ***\n')
    print(knitr::kable(eltim,format = 'markdown'))
    sink()
    
    png(file=paste0('simulation/',case,'/diagnostics/traces_',model_tag,'.png'),
        width=800,height=800)
    print(traceplot(fit, pars=c(parOpt,'lp__'), inc_warmup=TRUE, cex=2))
    dev.off()
    
    png(file=paste0('simulation/',case,'/diagnostics/pairs_',model_tag,'.png'),
        width=800,height=800)
    pairs(fit, pars=c(parOpt,'lp__'), gap=0, cex=2)
    dev.off()
    
    rm(fit)
  }
}

# Generate figures ####
source('code/functions/build_figures.R')

# Generate tables ####
source('code/functions/build_tables.R')
