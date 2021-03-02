
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
SIMgame <- function(t1, t2, yr, pred_wins=T, statsTBL=statsTBL, cv_outcome=cv_outcome){
  matchup <- left_join(statsTBL %>% filter(year==yr & Team==t1),
                       statsTBL %>% filter(year==yr & Team==t2),
                       by='year', suffix=c('','_2'))
  # x <- as.matrix(select(matchup, AdjEM, AdjO, AdjD, AdjT, Luck, OppO, OppD,
  #                       AdjEM_2, AdjO_2, AdjD_2, AdjT_2, Luck_2, OppO_2, OppD_2,
  #                       Rk, Losses, Rk_2, Losses_2))
  x <- as.matrix(select(matchup, AdjEM, AdjO, AdjD, AdjT, OppO, OppD,
                        AdjEM_2, AdjO_2, AdjD_2, AdjT_2, OppO_2, OppD_2,
                        Losses, Losses_2))
  if (pred_wins==T){
    out <- predict(cv_outcome, newx=x, s="lambda.min", type="response")[1]
  } 
  # else {
  #   out <- predict(cv_upset, newx=x, s="lambda.min", type="response")[1]
  # }
  return(out)
}

# # bootstrap probability
# SIMprob <- function(t1, t2, yr, pred_wins=T, bootlen=100){
#   s <- sample(x=c(t1,t2), size=bootlen, replace=T,
#               prob=c(SIMgame(t1=t1, t2=t2, yr=yr, pred_wins=pred_wins), 
#                      SIMgame(t1=t2, t2=t1, yr=yr, pred_wins=pred_wins)))
#   s2 <- sapply(unique(s), function(x) length(which(s==x)))
#   out <- ifelse(is.na(s2[t1]), 0, s2[t1]/bootlen) # account for no wins
#   return(out)
# }

# # find matchup from lut
# matchup <- function(tbl1, tbl2){
#   LUT %>% 
#     filter(team1==tbl1$Team & team2==tbl2$Team) %>% 
#     mutate(CumWinPct=tbl1$CumWinPct) %>% 
#     left_join(
#       LUT %>% 
#         filter(team1==tbl2$Team & team2==tbl1$Team) %>% 
#         mutate(CumWinPct=tbl2$CumWinPct),
#       by=c('team1'='team2', 'team2'='team1'),
#       suffix=c('_1','_2')) %>%
#     mutate(AdjwinP=round((winP_1*(1-upsetP_1)) - (winP_2), 2),
#            CumWinPct=ifelse(AdjwinP>0, winP_1+CumWinPct_1, winP_2+CumWinPct_2),
#            SOS=CumWinPct_1 - CumWinPct_2,
#            Advance=ifelse(AdjwinP>0 & SOS>=0, 1, 2))
# }

matchup <- function(tbl1, tbl2, lut=LUT){
  lut %>% 
    filter(team1==tbl1$Team & team2==tbl2$Team) %>% 
    mutate(CumWinPct=tbl1$CumWinPct) %>% 
    left_join(
      lut %>% 
        filter(team1==tbl2$Team & team2==tbl1$Team) %>% 
        mutate(CumWinPct=tbl2$CumWinPct),
      by=c('team1'='team2', 'team2'='team1'),
      suffix=c('_1','_2')) %>%
    mutate(Advance=sample(x=c(1, 2), size=1, prob=c(winP_1, winP_2)),
           CumWinPct=ifelse(Advance==1, winP_1+CumWinPct_1, winP_2+CumWinPct_2))
}


runRND <- function(teamsIN, rnd, yr){
  rndNM <- sym(paste0('RND',rnd))
  if (rnd<5){
    t <- teamsIN %>% mutate(id=paste0(Region, !!rndNM))
    if (!'CumWinPct' %in% names(t)) { t$CumWinPct <- 0 }
    l <- lapply(t %>% pull(id) %>% unique(), function(x){
      highSD <- t %>% filter(id==x) %>% slice_min(Seed)
      lowSD <- t %>% filter(id==x) %>% slice_max(Seed)
      SIMgame(t1=highSD$Team, t2=lowSD$Team, yr=yr)
      matchup(tbl1=highSD, tbl2=lowSD)
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
      matchup(tbl1=highSD, tbl2=lowSD)
    }) %>% 
      do.call('rbind',.) %>% 
      as.data.frame()
  }
  teamsOUT <- l %>% 
    mutate(Team=ifelse(Advance==1, team1, team2),
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
mkBracket <- function(yr, mt=masterTBL){
  teamsTBL <- filter(masterTBL, year==yr)
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
