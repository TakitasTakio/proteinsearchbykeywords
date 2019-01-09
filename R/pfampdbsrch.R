# Search pfam and pdb data from protein function keywords!
#
#
#install.packages("devtools")
#library(devtools)
#library(roxygen2)
install.packages("rvest")
library(rvest)
library(bio3d)
library(PFAM.db)
library(stringr)

obtain_from_keywords <- function(keywords="immunity"){
  if(is.null(keywords) ){
    warning("Please enter keywords as character string, e.g. 'immunity'")
  }
  if(is.numeric(keywords)){
    warning("Keywords cannot be numeric, please enter keywords as character string, e.g. 'immunity'")
  }
  if(str_detect(keywords, "\\s")){
    warning("If your keywors include whitespace in it, please use '+' instead of whitespace , e.g. the format should be like 'heart+disease'")
  }

  else {
    url.start="https://pfam.xfam.org/search/keyword?query="
    url.end="&submit=Submit"
    url.1=paste(url.start, keywords, url.end, sep = "")
    md=url.1%>% read_html(encoding = "UTF-8") %>% html_nodes("table.resultTable")
    md.pfam=md%>%html_nodes("tbody>tr>td:nth-child(2)") %>%html_text
    md.id=md%>%html_nodes("tbody>tr>td:nth-child(3)") %>%html_text
    md.dsp=md%>%html_nodes("tbody>tr>td:nth-child(4)") %>%html_text
    defense.data=data.frame(md.pfam, md.id, md.dsp)
    colnames(defense.data)=c('pfam-ACCN', 'ID', 'Description')

    print(defense.data)
  }
}


obtain_pdb_from_keyword = function(searchItem="immunity") {
  if(is.null(searchItem) ){
    warning("Please enter keywords as character string, e.g. 'immunity'")
  }
  if(is.numeric(searchItem)){
    warning("Keywords cannot be numeric, please enter keywords as character string, e.g. 'immunity'")
  }
  if(str_detect(searchItem, "\\s")){
    warning("If your keywors include whitespace in it, please use '+' instead of whitespace , e.g. the format should be like 'heart+disease'")
  }

  else{
    url.start="https://pfam.xfam.org/search/keyword?query="
    url.end="&submit=Submit"
    url.1=paste(url.start, searchItem, url.end, sep = "")
    md=url.1%>% read_html(encoding = "UTF-8") %>% html_nodes("table.resultTable")
    md.pfam=md%>%html_nodes("tbody>tr>td:nth-child(2)") %>%html_text
    md.id=md%>%html_nodes("tbody>tr>td:nth-child(3)") %>%html_text
    md.dsp=md%>%html_nodes("tbody>tr>td:nth-child(4)") %>%html_text
    defense.data=data.frame(md.pfam, md.id, md.dsp)
    colnames(defense.data)=c('pfam-ACCN', 'ID', 'Description')

    # new.pp.lib=read.delim(file = "data/pdb_pfam_mapping.txt")
    #
    # new.pp.lib$PFAM_ACC_ABS=str_sub(new.pp.lib$PFAM_ACC,1,7)
    #
    # defense.data$pdb.id=1
    # defense.data$pdb.chain=1
    # for(i in 1:nrow(defense.data)){
    # defense.data$pdb.id[i]=new.pp.lib$PDB_ID[new.pp.lib$PFAM_ACC_ABS==defense.data$`pfam-ACCN`[i]]
    # defense.data$pdb.chain[i]=new.pp.lib$CHAIN_ID[new.pp.lib$PFAM_ACC_ABS==defense.data$`pfam-ACCN`[i]]
    # }

    pfam.to.pdb_no= data.frame(PFAMPDB)
    conv.to.pdb=function(x){
    return(
      pfam.to.pdb_no$pdb[pfam.to.pdb_no$ac==x]
    )
  }
  defense.data$pdb_id=defense.data$`pfam-ACCN`
  defense.pdb=lapply(defense.data$pdb_id, conv.to.pdb)
  print(defense.pdb)
     ########"The pdb list order indexes correspond to the order index number of pfams, please use obtain_full_pfampdb() to get the full table"
}
}

obtain_full_pfampdb=function(item="immunity"){
  if(is.null(item) ){
    warning("Please enter keywords as character string, e.g. 'immunity'")
  }
  if(is.numeric(item)){
    warning("Keywords cannot be numeric, please enter keywords as character string, e.g. 'immunity'")
  }
  if(str_detect(item, "\\s")){
    warning("If your keywors include whitespace in it, please use '+' instead of whitespace , e.g. the format should be like 'heart+disease'")
  }
  else{
    url.start="https://pfam.xfam.org/search/keyword?query="
    url.end="&submit=Submit"
    url.1=paste(url.start, item, url.end, sep = "")
    md=url.1%>% read_html(encoding = "UTF-8") %>% html_nodes("table.resultTable")
    md.pfam=md%>%html_nodes("tbody>tr>td:nth-child(2)") %>%html_text
    md.id=md%>%html_nodes("tbody>tr>td:nth-child(3)") %>%html_text
    md.dsp=md%>%html_nodes("tbody>tr>td:nth-child(4)") %>%html_text
    defense.data=data.frame(md.pfam, md.id, md.dsp)
    colnames(defense.data)=c('pfam-ACCN', 'ID', 'Description')

    pfam.to.pdb_no= data.frame(PFAMPDB)
    conv.to.pdb=function(x){
      return(
        pfam.to.pdb_no$pdb[pfam.to.pdb_no$ac==x]
      )
    }
    defense.data$pdb_id=defense.data$`pfam-ACCN`
    defense.pdb=lapply(defense.data$pdb_id, conv.to.pdb)

      new.defense.pdb=defense.pdb
  for(i in 1:length(defense.pdb)){
    new.defense.pdb[i]= paste(defense.pdb[i], collapse = '')
  }
  new.defense.pdb=unlist(new.defense.pdb)
  for(i in 1:length(new.defense.pdb)){
    new.defense.pdb[i]=gsub("[^[:upper:][:digit:][:blank:]+,?&/\\-]", "",new.defense.pdb[i])
  }

  new.defense.pdb[new.defense.pdb=="0"]="NA"
  defense.data$pdb_id=new.defense.pdb
  print(defense.data)

  }
}





