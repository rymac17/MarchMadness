
# function for scraping ranking tables off kenpom
scrape_kenpom <- function(yr){
  require(rvest)
  require(magrittr)
  require(dplyr)
  site <- paste0('https://kenpom.com/index.php?y=',yr)
  # pull html table
  tbl <- read_html(site) %>% 
    html_nodes(., 'table') %>% 
    html_table(., fill=TRUE, header=F) %>% 
    .[[1]]
  # rename columns
  colnames(tbl) <- tbl[2,] %>% unlist()
  # remove extra ranks
  tbl <- tbl[,-c(7,9,11,13,15,17,19,21)]
  # remove old headers
  tbl <- tbl %>% filter(Rk!='' & Rk!='Rk')
  # change data type
  tbl <- tbl %>% 
    mutate(Rk=as.numeric(Rk),
           Team=fixNM(Team),
           Conf=Conf,
           Wins=splitWL(`W-L`),
           Losses=splitWL(`W-L`, rtnW=F),
           AdjEM=getSIGN(AdjEM),
           AdjO=as.double(AdjO),
           AdjD=as.double(AdjD),
           AdjT=as.double(AdjT),
           Luck=getSIGN(Luck),
           OppO=as.double(OppO),
           OppD=as.double(OppD),
           `W-L`=NULL,
           .keep='used'
    ) %>% 
    mutate(year=yr)
  write.csv(tbl, paste0('data/kenpom/kenpom',yr,'.csv'), row.names=F)
}

# split win-loss column into wins and losses
splitWL <- function(wl, rtnW=T){
  require(stringr)
  sapply(wl, function(x){
    ss <- str_split(string=x, pattern='-', simplify=T)
    w <- as.numeric(ss[1,1])
    l <- as.numeric(ss[1,2])
    if (rtnW==T){
      return(w)
    } else (return(l))
  }, USE.NAMES=F)
}

# convert character +/- into double
getSIGN <- function(pm){
  sapply(pm, function(x){
    if (grepl('[-]',x)){ return(0 - gsub('[-]','',x) %>% as.double()) }
    if (grepl('[+]',x)){ return(gsub('[-]','',x) %>% as.double()) }
  }, USE.NAMES=F)
}

# remove name rank and punct
fixNM <- function(s){
  sapply(s, function(x){
    out <- gsub("[*'.0-9]", '', x) %>% trimws
    if (grepl('Saint', out, fixed=T)){ out <- gsub('Saint', 'St', out, fixed=T) }
    if (grepl('Mississippi', out, fixed=T)){ out <- 'Ole Miss'}
    if (grepl('Miami FL', out, fixed=T)){ out <- 'Miami'}
    if (grepl('Miami OH', out, fixed=T)){ out <- 'Miami Ohio'}
    if (grepl('Middle Tennessee', out, fixed=T)){ out <- 'Middle Tennessee St'}
    if (grepl('Central Connecticut', out, fixed=T)){ out <- 'Central Connecticut St'}
    if (grepl('Penn', out, fixed=T)){ out <- 'Pennsylvania'}
    if (grepl('Troy St', out, fixed=T)){ out <- 'Troy'}
    if (grepl('Southern Miss', out, fixed=T)){ out <- 'Southern Mississippi'}
    if (grepl('Green Bay', out, fixed=T)){ out <- 'Wisconsin Green Bay'}
    
    return(out)
  }, USE.NAMES=F)
}
