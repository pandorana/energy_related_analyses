# Script for ensuring the raw data sets are ready
stop <- function(info) {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  base::stop(info)
}

if (is.na(match("a_hh" , ls()))) {
  if (is.na(match("a_hhresp.tab" , list.files("data")))) {
    stop("Please put file a_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    a_hh <- read.delim("data/a_hhresp.tab") %>% as.tibble()
    cache("a_hh")
  }
}

if (is.na(match("b_hh" , ls()))) {
  if (is.na(match("b_hhresp.tab" , list.files("data")))) {
    stop("Please put file b_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    b_hh <- read.delim("data/b_hhresp.tab") %>% as.tibble()
    cache("b_hh")
  }
}

if (is.na(match("c_hh" , ls()))) {
  if (is.na(match("c_hhresp.tab" , list.files("data")))) {
    stop("Please put file c_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    c_hh <- read.delim("data/c_hhresp.tab") %>% as.tibble()
    cache("c_hh")
  }
}

if (is.na(match("d_hh" , ls()))) {
  if (is.na(match("d_hhresp.tab" , list.files("data")))) {
    stop("Please put file d_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    d_hh <- read.delim("data/d_hhresp.tab") %>% as.tibble()
    cache("d_hh")
  }
}

if (is.na(match("e_hh" , ls()))) {
  if (is.na(match("e_hhresp.tab" , list.files("data")))) {
    stop("Please put file e_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    e_hh <- read.delim("data/e_hhresp.tab") %>% as.tibble()
    cache("e_hh")
  }
}

if (is.na(match("f_hh" , ls()))) {
  if (is.na(match("f_hhresp.tab" , list.files("data")))) {
    stop("Please put file f_hhresp.tab from Understanding Society Survey under folder data/")
  }
  else {
    f_hh <- read.delim("data/f_hhresp.tab") %>% as.tibble()
    cache("f_hh")
  }
}

if (is.na(match("lcf_expen" , ls()))) {
  if (is.na(match("2013_dv_set89_ukanon.tab" , list.files("data")))) {
    stop("Please put file 2013_dv_set89_ukanon.tab from Living Cost & Food Survey under folder data/")
  }
  else {
    lcf_expen <- read.delim("data/2013_dv_set89_ukanon.tab") %>% as.tibble()
    cache("lcf_expen")
  }
}

if (is.na(match("lcf_data" , ls()))) {
  if (is.na(match("2013_dvhh_ukanon.tab" , list.files("data"))) | 
      is.na(match("2013_rawhh_ukanon.tab", list.files("data")))) {
    stop("Please ensure files 2013_dvhh_ukanon.tab and 2013_rawhh_ukanon.tab 
         from Living Cost & Food Survey are under folder data/")
  }
  else {
    lcf_data <- read.delim("data/2013_dvhh_ukanon.tab")
    names(lcf_data) <- tolower(names(lcf_data))
    lcf_data <- lcf_data %>% cbind(read.delim("data/2013_rawhh_ukanon.tab") %>% select(CHFuel)) %>% as.tibble()
    cache("lcf_data")
  }
}

rm(stop)
