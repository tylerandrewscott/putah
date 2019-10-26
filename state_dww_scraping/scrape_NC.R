nc_base = 'https://www.pwss.enr.state.nc.us/NCDWW2/JSP/SearchDispatch?number=&name=&companyname=&WaterSystemStatusCode=All&county=All&WaterSystemType=C&SourceWaterType=All&PointOfContactType=None&SampleType=null&begin_date=9%2F12%2F2016&end_date=9%2F12%2F2018&action=Search+For+Water+Systems'
system_base = "https://www.pwss.enr.state.nc.us/NCDWW2/JSP/"
library(rvest)
library(tidyverse)
ta = nc_base %>% read_html()
base_table =  html_nodes(ta,'table')
base_table =  html_table(base_table[[2]],trim=T,fill=T) 
colnames(base_table) <- base_table[1,]
base_table = base_table[-1,]
colnames(base_table) = gsub('\n|\r','',colnames(base_table))
colnames(base_table) = gsub('\\s{2,}',' ',colnames(base_table))
base_table$PWS_ID = base_table$`Water System No.`

hrefs = ta %>% html_nodes('a') %>% html_attr('href')
hrefs = hrefs[grepl('WaterSystemDetail',hrefs)]
base_table$href = hrefs
base_table$href = gsub('\\s{1,}$','',paste0(system_base,base_table$href)) 
colnames(base_table) <- gsub('\\s{2,}',' ',colnames(base_table))
base_table = base_table %>% mutate(PWS_ID = `Water System No.`)
library(parallel)

temp = lapply(seq_along(base_table$href),function(x) 
{base_table$href[x] %>% read_html() %>% 
  html_nodes('table') %>% html_table(fill=T,trim=T)})

basic_system_attributes_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = data.frame(matrix(c(temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,2],temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,4]),nrow=1),stringsAsFactors = F)
  colnames(tdf) <- c(temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,1],temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,3])
  tdf$PWS_ID = tdf$`Water System No.:`
  tdf}))


operators_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Water System Contacts",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[,!is.na(colnames(tdf))]
  tdf = tdf[-1,]
  tdf}))

system_sources_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Sources of Water",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  tdf$PWS_ID = base_table$PWS_ID[x]
  tdf}))

system_purchases_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Purchases",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  tdf$PWS_ID = base_table$PWS_ID[x]
  tdf}))

service_areas_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Service Area",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf}))

service_population_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Annual Operating Period",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf}))

service_connections_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Service Connections",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf}))

pws_list = list(pws_master_list = base_table,
                basic_info = basic_system_attributes_df,
                service_population = service_population_df,
                service_connections = service_connections_df,
     services = service_areas_df, operators = operators_df,
     water_sources = system_sources_df,purchasing_connections = system_purchases_df)

saveRDS(pws_list,paste0('input/state_dww_database_records/NC_',Sys.Date(),'_PWS.RDS'))

