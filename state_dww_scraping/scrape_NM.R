nm_base = 'https://dww.water.net.env.nm.gov/DWW/JSP/SearchDispatch?number=&name=&county=All&WaterSystemType=All&SourceWaterType=All&PointOfContactType=None&SampleType=null&begin_date=8%2F28%2F2016&end_date=8%2F28%2F2018&action=Search+For+Water+Systems'
system_base = "https://dww.water.net.env.nm.gov/DWW/JSP/"
library(rvest)
library(tidyverse)
ta = nm_base %>% read_html()
base_table =  html_nodes(ta,'table')
base_table =  html_table(base_table[[2]],trim=T,fill=T) 
colnames(base_table) <- base_table[1,]
base_table = base_table[-1,]
colnames(base_table) = gsub('\n|\r','',colnames(base_table))
colnames(base_table) = gsub('\\s{2,}',' ',colnames(base_table))
base_table$PWS_ID = base_table$`Water System No.`


hrefs = ta %>% html_nodes('a') %>% html_attr('href')
hrefs = hrefs[grepl('WaterSystemDetail',hrefs)]
hrefs = gsub(' ','',hrefs)
base_table$href = hrefs
base_table$href = gsub('\\s{1,}$','',paste0(system_base,base_table$href)) 
colnames(base_table) <- gsub('\\s{2,}',' ',colnames(base_table))
base_table = base_table %>% mutate(PWS_ID = `Water System No.`)


library(parallel)
temp = lapply(seq_along(base_table$href),function(x) 
{temp_tabs = base_table$href[x] %>% read_html() %>% html_nodes('table')
temp_tabs = lapply(temp_tabs,function(x) tryCatch(html_table(x,fill=T,trim=T),error=function(e) NULL))
temp_tabs})


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
  tdf = tdf[rowSums(is.na(tdf))==0,]
  tdf}))

system_sources_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Sources of Water",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0){tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf}))

system_purchases_df = do.call(rbind,lapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Water Purchases",temp[[x]]))]]
  if(ncol(tdf)==1){
  tdf = tdf[-1,]
  tdf = data.frame(tdf,stringsAsFactors = F)
  tdf$PWS_ID = base_table$PWS_ID[x]
  tdf}}))
system_purchases_df$SELLER_ID = gsub('^ ','',str_extract(system_purchases_df$tdf," NM[0-9]{1,}"))
system_purchases_df$STATUS = gsub('\r|\n|\t| &nbsp','',str_extract(system_purchases_df$tdf,"[^\\/]{1,}$"))
system_purchases_df$SELLER_NAME = gsub(" - NM.*",'',gsub('buys from ','',str_extract(system_purchases_df$tdf,"buys from.+NM[0-9]{1,}")))



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

saveRDS(pws_list,paste0('input/state_dww_database_records/NM_',Sys.Date(),'_PWS.RDS'))

