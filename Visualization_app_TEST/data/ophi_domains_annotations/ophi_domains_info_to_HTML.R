# ophi domains to html list
path_data <-"C:/Users/javie/Documents/CIB/JGF/Project_OphiKT/RNAseq/Scripts/shiny_visual_app_online/data/ophi_domains_annotations/"
file <- "domaininfo_ExternalModels.tab"
path <- paste0(path_data,file)
ophi_domains_file <- unique(read.csv(path, header = T,sep="\t"))
ophi_domains_file <- subset(ophi_domains_file,ophi_domains_file$domainDesc != "\\N")

ophi_domains_id <- ophi_domains_file$X.proteinId
ophi_domains_info <- paste0(ophi_domains_file$domainDesc," - ",
                            ophi_domains_file$domainId," - ",
                            ophi_domains_file$domainDb)
ophi_domains <-as.data.frame(cbind(ophi_domains_id,ophi_domains_info))
colnames(ophi_domains) <- c("id","domain_info")
Id_new_df <- data.frame()
for(i in 1:length(unique(ophi_domains$id))){
  
  Id <- unique(ophi_domains$id)[i]
  #test <- ophi_domains[ophi_domains$id == Id]
  test <- ophi_domains %>% filter(id == Id)
  
  print(paste0("row = ",i," Id = ",Id))
  
  data <- c()
  for(n in 1:ncol(test)){
    
    if(length(unique(test[,n]))>1){
      
      list_elements <- test[,n]
      list_elements <- unique(list_elements)
      list_elements <-  list_elements[!list_elements %in% "N/A"]
      #list_elements <- replace(list_elements, list_elements=="N/A", NULL)
      list_elements <- paste0(paste0("<li>",list_elements,"</li>"),collapse = "")
      
      list_ul <- paste0( "<ul>",list_elements,"</ul>" )
      data <- append(data,list_ul)
      
    }else{
      data <- append(data,unique(test[,n]))
    }
    
    
    
  }
  data <- t(as.data.frame(data))
  colnames(data) <- colnames(ophi_domains)
  
  Id_new_df <- rbind(Id_new_df,data)
  
}

write.table(Id_new_df, file = paste0(path_data,"domain_info_ophi_html.tsv"), row.names=FALSE, sep="\t")