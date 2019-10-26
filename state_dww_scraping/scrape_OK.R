ok_base = 'http://sdwis.deq.state.ok.us/DWW/JSP/SearchDispatch?number=&name=&county=All&WaterSystemType=All&SourceWaterType=All&PointOfContactType=None&SampleType=null&begin_date=9%2F11%2F2016&end_date=9%2F11%2F2018&action=Search+For+Water+Systems'
system_base = "http://sdwis.deq.state.ok.us/DWW/JSP/"
library(rvest)
library(tidyverse)
ta = ok_base %>% read_html()
#base_table = ta %>% html_nodes('table') %>% html_table(trim=T,fill=TRUE)
tryCatch(base_table <- ta %>% html_nodes('table') %>% html_table(trim=T,fill=TRUE),
         error=function(e) print("OK site appears to have changed."))

colnames(base_table[[2]]) <- gsub('\n|\r','',base_table[[2]][1,])
base_table = base_table[[2]][-1,]
hrefs = ta %>% html_nodes('a') %>% html_attr('href')
hrefs = hrefs[grepl('WaterSystemDetail',hrefs)]
base_table$href = hrefs

base_table$href = gsub('\\s{1,}$','',paste0(system_base,base_table$href)) 
colnames(base_table) <- gsub('\\s{2,}',' ',colnames(base_table))
base_table = base_table %>% mutate(PWS_ID = `Water System No.`)

html_list = lapply(seq_along(base_table$href),function(x) base_table$href[x] %>% read_html())

system_sources_and_purchases = lapply(html_list,function(x) x %>% html_nodes('#AutoNumber7'))
system_sources_df = do.call(rbind,lapply(seq_along(system_sources_and_purchases),function(x) 
  system_sources_and_purchases[[x]][[1]] %>% html_table(trim=T) %>% mutate(PWS_ID = base_table$PWS_ID[x])))
colnames(system_sources_df) <- gsub('\r|\n','',colnames(system_sources_df))
colnames(system_sources_df) <- gsub('\\s{2,}',' ',colnames(system_sources_df))

system_purchases_df = do.call(rbind,lapply(seq_along(system_sources_and_purchases),function(x) 
  system_sources_and_purchases[[x]][[2]] %>% html_table(trim=T) %>% mutate(PWS_ID = base_table$PWS_ID[x])))
colnames(system_purchases_df) <- gsub('\r|\n','',colnames(system_purchases_df))
colnames(system_purchases_df) <- gsub('\\s{2,}',' ',colnames(system_purchases_df))


css_operators = '#AutoNumber2'
operators_df = do.call(rbind,lapply(seq_along(html_list),function(x) html_list[[x]] %>% 
                                      html_nodes(css_operators) %>% html_table(trim=T) %>%
                                      .[[1]] %>% mutate(PWS_ID = base_table$PWS_ID[x])))

css_service_areas= '#AutoNumber9'
service_areas_df = do.call(rbind,lapply(seq_along(html_list),function(x) html_list[[x]] %>% 
                                      html_node(css_service_areas) %>% html_table(trim=T) %>%
                                      mutate(PWS_ID = base_table$PWS_ID[x],Code = ifelse(Code==TRUE,'T',Code))))

css_service_population= '#AutoNumber4'
service_population_df = do.call(rbind,lapply(seq_along(html_list),function(x) html_list[[x]] %>% 
                                          html_nodes(css_service_population) %>% html_table(trim=T) %>%
                                          .[[1]] %>% mutate(PWS_ID = base_table$PWS_ID[x])))
service_population_df$`Population Type`[service_population_df$`Population Type`=='TRUE'] <- 'T'


css_service_connections= '#AutoNumber5'
service_connections_df = do.call(rbind,lapply(seq_along(html_list),function(x) html_list[[x]] %>% 
                                               html_nodes(css_service_connections) %>% html_table(trim=T) %>%
                                               .[[1]] %>% mutate(PWS_ID = base_table$PWS_ID[x])))

css_basic_titles = '#AutoNumber10 b font'
css_basic_values = '#AutoNumber10 td > font'

basic_system_attributes_df = do.call(rbind,lapply(seq_along(html_list),function(x) {
      names = html_list[[x]] %>% html_nodes(css_basic_titles) %>% html_text(trim=T)
      vals =  html_list[[x]] %>% html_nodes(css_basic_values) %>% html_text(trim=T)
      tdf = data.frame(t(data.frame(vals)),stringsAsFactors = F)
      colnames(tdf) <- names
      tdf$PWS_ID = tdf$`Water System No. :`
      rownames(tdf) <- 1:nrow(tdf)
      tdf}))

pws_list = list(pws_master_list = base_table,
                basic_info = basic_system_attributes_df,
                service_population = service_population_df,
                service_connections = service_connections_df,
     services = service_areas_df, operators = operators_df,
     water_sources = system_sources_df,purchasing_connections = system_purchases_df)

saveRDS(pws_list,paste0('input/state_dww_database_records/OK_',Sys.Date(),'_PWS.RDS'))

