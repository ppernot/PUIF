# Build all figures

for (model_tag in list_meth) {
  fit_tag = paste0("fit",model_tag)
  load(paste0('simulation/',case,'/',fit_tag,'.rda'))
  
  plot_all(legend=list_legends[model_tag])
  
}