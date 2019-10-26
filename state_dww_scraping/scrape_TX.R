library(rvest)
library(tidyverse)
library(parallel)
library(pbapply)

base_page = "https://dww2.tceq.texas.gov/DWW/"
base_session = html_session(base_page)
county_options = base_session %>% read_html() %>% html_nodes("select[name='county'] > option") %>% html_attr('value')
county_options = county_options[county_options!='All']
county_options = gsub(' ','%20',county_options)
tx_base_start = 'https://dww2.tceq.texas.gov/DWW/JSP/SearchDispatch?number=&name=&ActivityStatusCD=All&county='
tx_base_end = '&WaterSystemType=All&SourceWaterType=All&SampleType=null&begin_date=10%2F1%2F2016&end_date=10%2F1%2F2018&action=Search+For+Water+Systems'

county_pages = paste0(tx_base_start,county_options,tx_base_end)
system_base = "https://dww2.tceq.texas.gov/DWW/JSP/"


county_results_list = lapply(county_pages,function(page){
ta = page %>% read_html()
base_table = ta %>% html_nodes('table') %>% html_table(trim=T,fill=TRUE)
name_set =  base_table[[3]][1,]
base_table = base_table[[3]][-1,]
colnames(base_table) <- name_set
hrefs = ta %>% html_nodes('a') %>% html_attr('href')
hrefs = hrefs[grepl('WaterSystemDetail',hrefs)]
base_table$href = hrefs
base_table$href = gsub('\\s','',paste0(system_base,base_table$href))
base_table$poc_hrefs = gsub("WaterSystemDetail.jsp","WSPOC.jsp",base_table$href)
base_table$oper_hrefs = gsub("WaterSystemDetail.jsp","WSOPS.jsp",base_table$href)
base_table$PWS_ID = base_table$`Water System No.`


#html_list = pblapply(seq_along(base_table$href),function(x) {Sys.sleep(0.2);base_table$href[x] %>% read_html()},cl = 10)
library(parallel)
temp = pblapply(seq_along(base_table$href),function(x) 
{
temp_tabs = base_table$href[x] %>% read_html() %>% html_nodes('table')
table_list = lapply(seq_along(temp_tabs),function(tb){
  ttab = tryCatch(html_table(temp_tabs[[tb]],fill=F,trim=T),error=function(e) NULL)
  if(is.null(ttab)){NULL}
  else if (ncol(ttab)>20){NULL}
  else{ttab}
  })
table_list
},cl = 10)

if(length(temp)!=1)
{temp = temp[-1]}

basic_system_attributes_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = data.frame(matrix(c(temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,2],temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,4]),nrow=1),stringsAsFactors = F)
  colnames(tdf) <- c(temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,1],temp[[x]][[max(grep("Water System Detail Information",temp[[x]]))]][,3])
  colnames(tdf) <- gsub('\n|\t|\r','',colnames(tdf))
   tdf$PWS_ID = tdf$`WaterSystem No.:`
  tdf},cl = 10))

system_sources_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Sources of Water",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0){tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))


# system_purchases_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
#   tdf = temp[[x]][[max(grep("Water Purchases",temp[[x]]))]]
#   if(ncol(tdf)==1){
#     tdf = tdf[-1,]
#     tdf = data.frame(tdf,stringsAsFactors = F)
#     tdf$PWS_ID = base_table$PWS_ID[x]
#     tdf}
#   if(ncol(tdf)!=1){
#     tdf = tdf[-1,1]
#     tdf = tdf[tdf!='']
#     tdf = str_extract(tdf,'TX.*(?=\\r)')
#     tdf = data.frame(tdf,stringsAsFactors = F)
#     tdf$PWS_ID = base_table$PWS_ID[x]
#     tdf}
#   },cl = 10))

#system_purchases_df$SELLER_ID = str_extract(system_purchases_df$tdf," TX[0-9]{1,}")
#system_purchases_df$STATUS = gsub('providing ','',str_extract(system_purchases_df$tdf,"providing.*$"))
#system_purchases_df$SELLER_NAME = gsub('buys from ','',str_extract(system_purchases_df$tdf,"buys from.+\\w"))

service_areas_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Service Area",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf}, cl = 10))

service_population_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Annual Operating Period",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))


service_connections_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Service Connections",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))

service_connections_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("Service Connections",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))


storage_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("WS Measures",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))

flow_df = do.call(rbind,pblapply(seq_along(temp),function(x) {
  tdf = temp[[x]][[max(grep("WS Flow Rates",temp[[x]]))]]
  colnames(tdf) <- tdf[1,]
  tdf = tdf[-1,]
  if(nrow(tdf)>0)
  {tdf$PWS_ID = base_table$PWS_ID[x]}
  tdf},cl = 10))

# temp_oper = mclapply(seq_along(base_table$oper_hrefs),function(x) 
# {temp_tabs = base_table$oper_hrefs[x] %>% read_html() %>% html_nodes('table')
# temp_tabs = lapply(temp_tabs,function(x) tryCatch(html_table(x,fill=T,trim=T),error=function(e) NULL))
# Sys.sleep(0.25)
# temp_tabs},mc.preschedule = TRUE,mc.cores = 10,mc.cleanup = T)
# 
# operators_df = do.call(rbind,pblapply(seq_along(temp_oper),function(x) {
#   tdf = temp_oper[[x]][[max(grep("System Contacts",temp_oper[[x]]))]]
#   if(nrow(tdf)>1)
#   {
#   colnames(tdf) <- tdf[1,] 
#   tdf$PWS_ID = base_table$PWS_ID[x]
#   tdf}},cl = 10))


temp_poc = mclapply(seq_along(base_table$poc_hrefs),function(x) 
{temp_tabs = base_table$poc_hrefs[x] %>% read_html() %>% html_nodes('table')
temp_tabs = lapply(temp_tabs,function(x) tryCatch(html_table(x,fill=T,trim=T),error=function(e) NULL))
Sys.sleep(0.25)
temp_tabs},mc.preschedule = TRUE,mc.cores = 10,mc.cleanup = T)

gc()

poc_df = do.call(rbind,pblapply(seq_along(temp_poc),function(x) {
  tdf = temp_poc[[x]][[max(grep("System Contacts",temp_poc[[x]]))]]
  tdf = tdf[rowSums(is.na(tdf)) != ncol(tdf), ]
  if(nrow(tdf)>1){
  colnames(tdf) = tdf[1,]
  tdf = tdf[-1,]
  tdf[tdf==''] <- NA
  tdf = tdf[grepl('Owner|Contact',tdf$Type),]
  tdf$Type = gsub('\\s{2,}',' ',gsub('\r|\t|\n',' ',tdf$Type))
  tdf$Contact = gsub('\\s{2,}',' ',gsub('\r|\t|\n',' ',tdf$Contact))
  tdf$Communication = gsub('\\s{2,}',' ',gsub('\r|\t|\n',' ',tdf$Communication))
  if(any(is.na(colnames(tdf))))
  {
  tdf =    tdf[,!is.na(colnames(tdf))]
  }
  tdf$PWS_ID = base_table$PWS_ID[x]
  tdf}},cl = 10))

colnames(service_population_df)  = gsub('\\s{2,}',' ',gsub('\r|\n|\t',' ',colnames(service_population_df)))


pws_list = list(pws_master_list = base_table,
                basic_info = basic_system_attributes_df,
                service_population = service_population_df,
                service_connections = service_connections_df,
                services = service_areas_df, #operators = operators_df,
                poc = poc_df,
                flow = flow_df,storage = storage_df,
                water_sources = system_sources_df) #,purchasing_connections = system_purchases_df)
pws_list
})

keys <- unique(unlist(lapply(county_results_list, names)))


all_counties = lapply(keys[keys!='operators'],function(k) plyr::rbind.fill(lapply(county_results_list,function(df) df[[k]])))
names(all_counties) = keys[keys!='operators']
pws_full_list = all_counties

saveRDS(pws_full_list,paste0('input/state_dww_database_records/TX_',Sys.Date(),'_PWS.RDS'))

all_counties$pws_master_list$`Pri. Cnty Served`

