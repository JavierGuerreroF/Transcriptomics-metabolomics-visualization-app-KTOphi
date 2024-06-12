library(rvest)
library(readr)
library(stringr)




path_data <- "C:/Users/javie/Documents/CIB/Saioa/SMM/Data JGI/Ophiostoma/"
path_results <- "C:/Users/javie/Documents/CIB/JGF/Project_OphiKT/RNAseq/Scripts/shiny_visual_app_online/data/Parse_JGI/"
file <- "OphpiCECT20416_2_GeneCatalog_genes_20210731.gff3"


ophi_gff <- read_tsv(paste0(path_data,file),
                    skip=2,
                    show_col_types = F,
                    col_names = F)

genes_row <- which(ophi_gff[,3]=="gene")

char_x9 <- as.vector(ophi_gff[genes_row,9][[1]])
prot_list <- str_split(char_x9,";")

prot_df <- data.frame(matrix(unlist(prot_list), nrow=length(prot_list), byrow=T))
proteinIDs <- gsub("proteinId=","", prot_df$X4) 


# WORKING
mainPageURL <- "https://signon.jgi.doe.gov/signon"

#create a web session with the desired login address
pgsession<-html_session(mainPageURL)
pgform<-html_form(pgsession)[[1]] #in this case the submit is the 2nd form
filled_form<-set_values(pgform, login="javiergueflo98@gmail.com", password="Pikacuhg_88")
submit_form(pgsession, filled_form)

print(">>> Start parsing...")
proteins_besthits <- data.frame()
for(i in proteinIDs){
  print(paste0("ProteinID: ",i))
  #ir a la pagina con el ID de la proteina
  page<-session_jump_to(pgsession, paste0("https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=",i))
  # buscar los elementos tr (en uno de ellos esta el Best hit)
  tr_elements <- html_nodes(page, "tr")
  # buscar el best
  best_hit <- tr_elements[grep("Best",tr_elements)]
  
  # modificaciones para quedarnos solo con el nombre del match
  a <- grep(">.*</a>",best_hit,value = T)
  if(length(a)>0){
  a <- str_match(a,">.*</a>")[1,1]
  a <- gsub("^>","",a)
  a <- gsub("</a>$","",a)
  a <- gsub("^.*\\| ","",a)
  
  b <- str_match(as.character(best_hit),"</a>.*" )
  b <- as.character(str_match(b,"\\(.*\\)"))
  
  name <- paste(a,b,sep="")
  }else{
    name <- NA
  }
  row <- append(i,name)
  proteins_besthits <- rbind(proteins_besthits, row)
  
  write.table(t(as.data.frame(row)), paste0(path_results,"best_hits_data.csv"),sep=",",col.names = F,row.names = F, append = T)
}
colnames(proteins_besthits) <- c("id","blastp_besthit")
write.csv(proteins_besthits, paste0(path_results,"best_hits_data_total.csv"), row.names=FALSE)

# 
# #########################
# #login
# url <- "https://signon.jgi.doe.gov/signon"
# session <- session(url)
# 
# form <- html_form(read_html(url))[[1]]
# 
# filled_form <- html_form_set(form,
#                           login = "javiergueflo98@gmail.com",
#                           password = "Pikacuhg_88")
# 
# session_submit(session, filled_form)
# 
# 
# # parser
# covidesa=xml2::read_html("https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=87215")
# 
# 
# 
# 
# 
# mainPageURL <- "https://signon.jgi.doe.gov/signon"
# mySession <- session(mainPageURL)
# 
# login <- mySession %>% 
#   session_jump_to("users/login.cgi") %>% 
#   html_element(".srbasic") %>%
#   html_form() %>% 
#   html_form_set(
#     username = "username",
#     password = "password"
#   )
# 
# 
# 
# # Fixup an unnamed field in the form object. Unnamed fields are not allowed and will cause an error.
# login$fields[[4]]$name <- "button"
# 
# # Set the action field to use the login.cgi program
# login$action <- "https://xxx.com/users/login.cgi"
# 
# # Create the session object
# logged_in <- mySession %>% session_submit(login)
# 
# 
# 
# 
# 
# library(rvest) 
# 
# #Address of the login webpage
# login<-"https://stackoverflow.com/users/login?ssrc=head&returnurl=http%3a%2f%2fstackoverflow.com%2f"
# 
# 
# # WORKING
# mainPageURL <- "https://signon.jgi.doe.gov/signon"
# 
# #create a web session with the desired login address
# pgsession<-html_session(mainPageURL)
# pgform<-html_form(pgsession)[[1]] #in this case the submit is the 2nd form
# filled_form<-set_values(pgform, login="javiergueflo98@gmail.com", password="Pikacuhg_88")
# submit_form(pgsession, filled_form)
# 
# 
# 
# 
# page<-session_jump_to(pgsession, "https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=87215")
# 
# 
# 
# 
# for(i in proteinIDs){
#   #ir a la pagina con el ID de la proteina
#   page<-session_jump_to(pgsession, paste0("https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=",i))
#   # buscar los elementos tr (en uno de ellos esta el Best hit)
#   tr_elements <- html_nodes(page, "tr")
#   # buscar el best
#   best_hit <- tr_elements[grep("Best",tr_elements)]
#   
#   # modificaciones para quedarnos solo con el nombre del match
#   a <- grep(">.*</a>",best_hit,value = T)
#   a <- str_match(a,">.*</a>")[1,1]
#   a <- gsub("^>","",a)
#   a <- gsub("</a>$","",a)
#   a <- gsub("^.*\\| ","",a)
# }
# 
# 
# ############### TESTEO
# html_nodes(page, "tr")
# 
# tr_elements <- html_nodes(page, "tr")
# best_hit <- tr_elements[grep("Best",tr_elements)]
# 
# library(stringr)
# a <- grep(">.*</a>",best_hit,value = T)
# 
# 
# a <- str_match(a,">.*</a>")[1,1]
# 
# 
# a <- gsub("^>","",a)
# a <- gsub("</a>$","",a)
# 
# a <- gsub("^.*\\| ","",a)
# 
# 
# ######
# 
# 
# bind_rows(lapply(html_attrs(tr_elements), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
# 
# library(purrr)
# html_nodes(page, "tr") %>% 
#   map(html_attr) %>% 
#   map_df(~as.list(.))
# 
# 
# text <- page %>% 
#   html_nodes(xpath='//*[@id="producto_tipo_producto_id"]')%>%
#   html_text() 
# text
# 
# #pre allocate the final results dataframe.
# results<-data.frame()  
# 
# #loop through all of the pages with the desired info
# for (i in 1:5)
# {
#   #base address of the pages to extract information from
#   url<-"http://stackoverflow.com/users/**********?tab=answers&sort=activity&page="
#   url<-paste0(url, i)
#   page<-jump_to(pgsession, url)
#   
#   #collect info on the question votes and question title
#   summary<-html_nodes(page, "div .answer-summary")
#   question<-matrix(html_text(html_nodes(summary, "div"), trim=TRUE), ncol=2, byrow = TRUE)
#   
#   #find date answered, hyperlink and whether it was accepted
#   dateans<-html_node(summary, "span") %>% html_attr("title")
#   hyperlink<-html_node(summary, "div a") %>% html_attr("href")
#   accepted<-html_node(summary, "div") %>% html_attr("class")
#   
#   #create temp results then bind to final results 
#   rtemp<-cbind(question, dateans, accepted, hyperlink)
#   results<-rbind(results, rtemp)
# }
# 
# #Dataframe Clean-up
# names(results)<-c("Votes", "Answer", "Date", "Accepted", "HyperLink")
# results$Votes<-as.integer(as.character(results$Votes))
# results$Accepted<-ifelse(results$Accepted=="answer-votes default", 0, 1)
# 
