ca_base = 'https://sdwis.waterboards.ca.gov/PDWW/JSP/SearchDispatch?number=&name=&county=&WaterSystemType=All&WaterSystemStatus=All&SourceWaterType=All&action=Search+For+Water+Systems'
library(rvest)
library(tidyverse)
ta = ca_base %>% read_html()
base_table = ta %>% html_nodes('table') %>% html_table(trim=T,fill=TRUE)
base_table = base_table[[2]][-1,]
hrefs = ta %>% html_nodes('a') %>% html_attr('href')
hrefs = hrefs[grepl('WaterSystemDetail',hrefs)]
base_table$href = hrefs
system_base = "https://sdwis.waterboards.ca.gov/PDWW/JSP/"
base_table$href = gsub('\\s{1,}$','',paste0(system_base,base_table$href)) 

html_list = lapply(seq_along(base_table$href),function(x) base_table$href[x] %>% read_html())

system_sources = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber7') %>% 
              html_table(trim=T,fill=T))
purchasing_connections = do.call(rbind,lapply(seq_along(system_sources),function(x) system_sources[[x]][[2]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(purchasing_connections) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(purchasing_connections)))

sources_df = do.call(rbind,lapply(seq_along(system_sources),function(x) system_sources[[x]][[1]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(sources_df) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(sources_df)))

sources_df = do.call(rbind,lapply(seq_along(system_sources),function(x) system_sources[[x]][[1]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(sources_df) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(sources_df)))

system_services = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber9') %>% 
                          html_table(trim=T,fill=T))
services_df = do.call(rbind,lapply(seq_along(system_services),function(x) system_services[[x]][[1]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(services_df) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(services_df)))

system_service_connections = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber5') %>% 
                           html_table(trim=T,fill=T))
service_connections_df = do.call(rbind,lapply(seq_along(system_service_connections),function(x) system_service_connections[[x]][[1]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(service_connections_df) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(service_connections_df)))


system_population = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber4') %>% 
                                      html_table(trim=T,fill=T))
population_df = do.call(rbind,lapply(seq_along(system_population),function(x) system_population[[x]][[2]] %>% mutate(PWS_ID = base_table$`Water System No.`[x])))
colnames(population_df) = gsub('\\s{2,}',' ',gsub('\n|\r','',colnames(population_df)))


cols = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber4 #AutoNumber6 td b') %>% html_text(trim=T))
vals = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber6 td:nth-child(2) font , #AutoNumber6 td:nth-child(4)',) %>% html_text(trim=T))

system_basic = do.call(rbind,lapply(seq_along(vals),function(x) {
  temp_df = data.frame(matrix(vals[x][[1]],nrow=1))
  colnames(temp_df) = cols[x][[1]]
  temp_df}))
system_basic$PWS_ID <- system_basic$`Water System No.`

base_table$PWS_ID = base_table$`Water System No.`
base_table = left_join(base_table,system_basic)

pws_list = list(pws_master_list = base_table,
                basic_info = system_basic,
                service_population = population_df,service_connections = service_connections_df,operators = NULL,
     services = services_df, water_sources = sources_df,purchasing_connections = purchasing_connections)

saveRDS(pws_list,paste0('input/state_dww_database_records/CA_',Sys.Date(),'_PWS.RDS'))

