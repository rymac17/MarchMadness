
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
    if (grepl('Mississippi$', out)){ out <- 'Ole Miss'}
    if (grepl('Penn$', out)){ out <- 'Pennsylvania'}
    if (grepl('Miami FL$', out)){ out <- 'Miami'}
    if (grepl('Miami OH$', out)){ out <- 'Miami Ohio'}
    if (grepl('Middle Tennessee$', out)){ out <- 'Middle Tennessee St'}
    if (grepl('Central Connecticut$', out)){ out <- 'Central Connecticut St'}
    if (grepl('Troy St$', out)){ out <- 'Troy'}
    if (grepl('Southern Miss$', out)){ out <- 'Southern Mississippi'}
    if (grepl('Green Bay$', out)){ out <- 'Wisconsin Green Bay'}
    if (grepl('UC Santa Barbara$', out)){ out <- 'Santa Barbara'}
    if (grepl('Milwaukee$', out)){ out <- 'Wisconsin Milwaukee'}
    if (grepl('UCF$', out)){ out <- 'Central Florida'}
    if (grepl('UT Arlington$', out)){ out <- 'Texas Arlington'}
    if (grepl('Texas A&M Corpus Chris$', out)){ out <- 'Texas A&M Corpus Christi'}
    if (grepl('LIU Brooklyn$', out)){ out <- 'Long Island Brooklyn'}
    if (grepl('Loyola MD$', out)){ out <- 'Loyola Maryland'}
    if (grepl('UTSA$', out)){ out <- 'Texas San Antonio'}
    return(out)
  }, USE.NAMES=F)
}

# simulate a game
# requires statsTBL and cv_outcome
SIMgame <- function(t1, t2, YR=yr){
  matchup <- left_join(statsTBL %>% filter(year==YR & Team==t1),
                       statsTBL %>% filter(year==YR & Team==t2),
                       by='year', suffix=c('','_2'))
  x <- as.matrix(matchup %>% dplyr::select(one_of(cv_outcome$glmnet.fit$beta@Dimnames[[1]]))) # use whatever vars model used
  out <- predict(cv_outcome, newx=x, s="lambda.min", type="response")[1]
  return(out)
}

SIMprob <- function(tbl1, tbl2, SS=sampleSize){
  highW <- SIMgame(t1=tbl1$Team, t2=tbl2$Team) # prob that high seed wins
  lowW <- SIMgame(t2=tbl1$Team, t1=tbl2$Team) # prob that low seed wins
  dft <- data.frame(team_1=tbl1$Team, team_2=tbl2$Team, 
                    CumWinPct_1=tbl1$CumWinPct, CumWinPct_2=tbl2$CumWinPct,
                    winP_1=highW, winP_2=lowW)
  if (SS==1){
    dft <- dft %>% 
      mutate(Advance=sample(x=c(1, 2), size=1, prob=c(winP_1, winP_2)),
             CumWinPct=ifelse(Advance==1, winP_1+CumWinPct_1, winP_2+CumWinPct_2))
  }
  if (SS>1){ # defaults to team 1 in a tie
    adv <- sample(x=c(1, 2), size=SS, replace=T, prob=c(dft$winP_1, dft$winP_2)) %>% 
      table(.)
    dft <- dft %>% 
      mutate(Advance=which.max(adv)[[1]],
             CumWinPct=ifelse(Advance==1, winP_1+CumWinPct_1, winP_2+CumWinPct_2))
  }
  return(dft)
}


runRND <- function(teamsIN, rnd){
  rndNM <- sym(paste0('RND',rnd))
  if (rnd<5){
    t <- teamsIN %>% mutate(id=paste0(Region, !!rndNM))
    if (!'CumWinPct' %in% names(t)) { t$CumWinPct <- 0 }
    l <- lapply(t %>% pull(id) %>% unique(), function(x){
      highSD <- t %>% filter(id==x) %>% slice_min(Seed)
      lowSD <- t %>% filter(id==x) %>% slice_max(Seed)
      SIMprob(tbl1=highSD, tbl2=lowSD) # requires yr to be defined
    }) %>% 
      do.call('rbind',.) %>% 
      as.data.frame()
  }
  if (rnd>=5){
    t <- teamsIN %>% mutate(id=!!rndNM)
    l <- lapply(t %>% pull(id) %>% unique(), function(x){
      bothSD <- t %>% filter(id==x)
      if (length(unique(bothSD$Seed))==1){
        highSD <- bothSD %>% slice_head(n=1)
        lowSD <- bothSD %>% slice_tail(n=1)
      } else { 
        highSD <- bothSD %>% slice_min(Seed)
        lowSD <- bothSD %>% slice_max(Seed)
      }
      SIMprob(tbl1=highSD, tbl2=lowSD)
    }) %>% 
      do.call('rbind',.) %>% 
      as.data.frame()
  }
  teamsOUT <- l %>% 
    mutate(Team=ifelse(Advance==1, team_1, team_2),
           CumWinPct=CumWinPct,
           .keep='none') %>% 
    left_join(., t, by='Team') %>% 
    mutate(CumWinPct=CumWinPct.x,
           round=rnd,
           .keep='unused') %>% 
    dplyr::select(-id, -CumWinPct.y)
  if (rnd==5){ teamsOUT$Region='Final Four' }
  if (rnd==6){ teamsOUT$Region='Championship' }
  return(teamsOUT)
}


# function to set up historic brackets
mkBracket <- function(YR=yr, mt=masterTBL){
  teamsTBL <- filter(mt, year==YR)
  teams <- rbind(
    teamsTBL %>% 
      filter(round==1) %>% 
      dplyr::select(Team=team, Region=region_name, Seed=seed, region_number),
    teamsTBL %>% 
      filter(round==1) %>% 
      dplyr::select(Team=team_2, Region=region_name, Seed=seed_2, region_number)
  ) %>% 
    mutate(RND1=case_when(Seed %in% c(1,16) ~ 'A',
                          Seed %in% c(2,15) ~ 'B',
                          Seed %in% c(3,14) ~ 'C',
                          Seed %in% c(4,13) ~ 'D',
                          Seed %in% c(5,12) ~ 'E',
                          Seed %in% c(6,11) ~ 'F',
                          Seed %in% c(7,10) ~ 'G',
                          Seed %in% c(8,9) ~ 'H'),
           RND2=case_when(Seed %in% c(1,16,8,9) ~ 'A',
                          Seed %in% c(2,15,7,10) ~ 'B',
                          Seed %in% c(3,14,6,11) ~ 'C',
                          Seed %in% c(4,13,5,12) ~ 'D'),
           RND3=case_when(Seed %in% c(1,16,8,9,4,13,5,12) ~ 'A',
                          Seed %in% c(2,15,7,10,3,14,6,11) ~ 'B'),
           RND4=case_when(region_number==1 ~ 'A',
                          region_number==2 ~ 'B',
                          region_number==3 ~ 'C',
                          region_number==4 ~ 'D'),
           RND5=case_when(region_number %in% c(1,2) ~'A',
                          region_number %in% c(3,4) ~'B'),
           RND6='A')
  return(teams)
}


# assign bracket position
# requires predDFT and teams
assignBRKT <- function(){
  mapBRKT <- rbind(
    teams %>%
      mutate(round=0, region_name=Region, winner=Team) %>%  # everyone's a winner
      dplyr::select(round, region_number, region_name, winner, Seed),
    predDFT) %>%
    mutate(BRKTcol=case_when(round==0 & region_number %in% c(1,2) ~ 1,
                             round==0 & region_number %in% c(3,4) ~ 13,
                             round==1 & region_number %in% c(1,2) ~ 2,
                             round==1 & region_number %in% c(3,4) ~ 12,
                             round==2 & region_number %in% c(1,2) ~ 3,
                             round==2 & region_number %in% c(3,4) ~ 11,
                             round==3 & region_number %in% c(1,2) ~ 4,
                             round==3 & region_number %in% c(3,4) ~ 10,
                             round==4 & region_number %in% c(1,2) ~ 5,
                             round==4 & region_number %in% c(3,4) ~ 9,
                             round==5 & region_number %in% c(1,2) ~ 6,
                             round==5 & region_number %in% c(3,4) ~ 8,
                             round==6 ~ 7),
           
           BRKTrow=case_when(round==0 & Seed==1 & region_number %in% c(1,3) ~ 1,
                             round==0 & Seed==16 & region_number %in% c(1,3) ~ 3,
                             round==0 & Seed==8 & region_number %in% c(1,3) ~ 5,
                             round==0 & Seed==9 & region_number %in% c(1,3) ~ 7,
                             round==0 & Seed==5 & region_number %in% c(1,3) ~ 9,
                             round==0 & Seed==12 & region_number %in% c(1,3) ~ 11,
                             round==0 & Seed==4 & region_number %in% c(1,3) ~ 13,
                             round==0 & Seed==13 & region_number %in% c(1,3) ~ 15,
                             round==0 & Seed==6 & region_number %in% c(1,3) ~ 17,
                             round==0 & Seed==11 & region_number %in% c(1,3) ~ 19,
                             round==0 & Seed==3 & region_number %in% c(1,3) ~ 21,
                             round==0 & Seed==14 & region_number %in% c(1,3) ~ 23,
                             round==0 & Seed==7 & region_number %in% c(1,3) ~ 25,
                             round==0 & Seed==10 & region_number %in% c(1,3) ~ 27,
                             round==0 & Seed==2 & region_number %in% c(1,3) ~ 29,
                             round==0 & Seed==15 & region_number %in% c(1,3) ~ 31,
                             
                             round==0 & Seed==1 & region_number %in% c(2,4) ~ 35,
                             round==0 & Seed==16 & region_number %in% c(2,4) ~ 37,
                             round==0 & Seed==8 & region_number %in% c(2,4) ~ 39,
                             round==0 & Seed==9 & region_number %in% c(2,4) ~ 41,
                             round==0 & Seed==5 & region_number %in% c(2,4) ~ 43,
                             round==0 & Seed==12 & region_number %in% c(2,4) ~ 45,
                             round==0 & Seed==4 & region_number %in% c(2,4) ~ 47,
                             round==0 & Seed==13 & region_number %in% c(2,4) ~ 49,
                             round==0 & Seed==6 & region_number %in% c(2,4) ~ 51,
                             round==0 & Seed==11 & region_number %in% c(2,4) ~ 53,
                             round==0 & Seed==3 & region_number %in% c(2,4) ~ 55,
                             round==0 & Seed==14 & region_number %in% c(2,4) ~ 57,
                             round==0 & Seed==7 & region_number %in% c(2,4) ~ 59,
                             round==0 & Seed==10 & region_number %in% c(2,4) ~ 61,
                             round==0 & Seed==2 & region_number %in% c(2,4) ~ 63,
                             round==0 & Seed==15 & region_number %in% c(2,4) ~ 65,
                             
                             round==1 & Seed %in% c(1,16) & region_number %in% c(1,3) ~ 2,
                             round==1 & Seed %in% c(8,9) & region_number %in% c(1,3) ~ 6,
                             round==1 & Seed %in% c(5,12) & region_number %in% c(1,3) ~ 10,
                             round==1 & Seed %in% c(4,13) & region_number %in% c(1,3) ~ 14,
                             round==1 & Seed %in% c(6,11) & region_number %in% c(1,3) ~ 18,
                             round==1 & Seed %in% c(3,14) & region_number %in% c(1,3) ~ 22,
                             round==1 & Seed %in% c(7,10) & region_number %in% c(1,3) ~ 26,
                             round==1 & Seed %in% c(2,15) & region_number %in% c(1,3) ~ 30,
                             round==1 & Seed %in% c(1,16) & region_number %in% c(2,4) ~ 36,
                             round==1 & Seed %in% c(8,9) & region_number %in% c(2,4) ~ 40,
                             round==1 & Seed %in% c(5,12) & region_number %in% c(2,4) ~ 44,
                             round==1 & Seed %in% c(4,13) & region_number %in% c(2,4) ~ 48,
                             round==1 & Seed %in% c(6,11) & region_number %in% c(2,4) ~ 52,
                             round==1 & Seed %in% c(3,14) & region_number %in% c(2,4) ~ 56,
                             round==1 & Seed %in% c(7,10) & region_number %in% c(2,4) ~ 60,
                             round==1 & Seed %in% c(2,15) & region_number %in% c(2,4) ~ 64,
                             
                             round==2 & Seed %in% c(1,16,8,9) & region_number %in% c(1,3) ~ 4,
                             round==2 & Seed %in% c(5,12,4,13) & region_number %in% c(1,3) ~ 12,
                             round==2 & Seed %in% c(6,11,3,14) & region_number %in% c(1,3) ~ 20,
                             round==2 & Seed %in% c(7,10,2,15) & region_number %in% c(1,3) ~ 28,
                             round==2 & Seed %in% c(1,16,8,9) & region_number %in% c(2,4) ~ 38,
                             round==2 & Seed %in% c(5,12,4,13) & region_number %in% c(2,4) ~ 46,
                             round==2 & Seed %in% c(6,11,3,14) & region_number %in% c(2,4) ~ 54,
                             round==2 & Seed %in% c(7,10,2,15) & region_number %in% c(2,4) ~ 62,
                             round==3 & Seed %in% c(1,16,8,9,5,12,4,13) & region_number %in% c(1,3) ~ 8,
                             round==3 & Seed %in% c(6,11,3,14,7,10,2,15) & region_number %in% c(1,3) ~ 24,
                             round==3 & Seed %in% c(1,16,8,9,5,12,4,13) & region_number %in% c(2,4) ~ 42,
                             round==3 & Seed %in% c(6,11,3,14,7,10,2,15) & region_number %in% c(2,4) ~ 58,
                             round==4 & region_number %in% c(1,3) ~ 16,
                             round==4 & region_number %in% c(2,4) ~ 50,
                             round==5 ~ 32,
                             round==6 ~ 32))
  BRKT <- matrix(ncol=13, nrow=65)
  for (i in 1:nrow(mapBRKT)){
    BRKT[mapBRKT[i,'BRKTrow'], mapBRKT[i,'BRKTcol']] <- mapBRKT[i,'winner']
  }
  BRKT[which(is.na(BRKT))] <- ''
  colnames(BRKT) <- c('First Round','Second Round','Sweet 16','Elite 8','Final Four','Final','Champion',
                      'Final', 'Final Four','Elite 8','Sweet 16','Second Round','First Round')
  return(BRKT)
}