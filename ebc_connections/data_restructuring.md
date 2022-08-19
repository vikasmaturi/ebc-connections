Restructuring of EBC correspondence data
================
2022-05-21

-   <a href="#setup" id="toc-setup">Setup</a>
    -   <a href="#libraries-and-data-import"
        id="toc-libraries-and-data-import">Libraries and data import</a>
-   <a href="#data-cleaning" id="toc-data-cleaning">Data cleaning</a>
-   <a href="#identify-prisons-that-are-incorrectly-labelled"
    id="toc-identify-prisons-that-are-incorrectly-labelled">Identify prisons
    that are incorrectly labelled</a>

## Setup

#### Libraries and data import

``` r
# load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.1.1

``` r
library(googlesheets4)
library(fuzzyjoin)
```

    ## Warning: package 'fuzzyjoin' was built under R version 4.1.3

``` r
# read in EBC Correspondence with people inside and families
raw_ebc <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1bneeDzC5VIUCakUGWb6O7mFW4jBaJWMn470zwf_zUx4/edit#gid=0", 
  sheet = "California State Prisons", 
  skip = 1,
  col_types = "ccccccccccccccccccccccccccc"
    )

# read in data on prisons
prison <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1lwhRr9Nwq8fKTSF4F-j_aLHcmZFLXQN7XjVJ2WJBWm8/edit#gid=1957970003", 
  sheet = "CA Prisons",
  col_types = "ccccccccc"
  )

# read in import template
import_template <- readxl::read_xlsx(path = "data_raw/crm_import_template.xlsx")
```

## Data cleaning

``` r
# small prison table for joining
prison_joining <-
  prison %>% 
  dplyr::select(`Prison Name`, `Prison Abbreviation`)
```

``` r
clean_ebc <-
  raw_ebc %>% 
  # clean column names and remove extraneous columns; label columns as old to disambiguate from new columsn added
  rename(
    "Housing_old" = Housing,
    "Address_old" = Address,
    "City_old" = City,
    "State_old" = State,
    "Zip_old" = Zip,
    "Enhancement info" = `Enhancement info (1, 3 = drug, 5, g = gun, GBI = great bodily injury, d = 1170 interest, x = took action on enhancement bills)`,
    "organizer_old" = organizer,
    "Notes" = Notes,
  ) %>% 
  select(-`Birthday Month`, -`...17`, -`...18`, -`...19`, -`...20`, -`...21`, -`...22`, -`...23`, -`...24`, -`...25`, -`...26`, -`...27`) %>% 
  # rename name columns to match import template
  rename(
    "First Name" = Name,
    "Last Name" = Last,
    "Prison ID Number" = Number, # Typically the CDCR number
    "Prison Abbreviation" = Prison,
  ) %>%
  # fix columns for later joining modification
  mutate(
    Address_old = stringr::str_replace(Address_old, stringr::fixed("PO Box"), stringr::fixed(stringr::fixed("P.O. Box"))),
    Address_old = stringr::str_replace(Address_old, stringr::fixed("PO BOX"), stringr::fixed(stringr::fixed("P.O. Box"))),
    Address_old = str_replace(Address_old, stringr::fixed("PO box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P. O. Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("PO BOx"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P.O Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("Po Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("po box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P.O. BOX"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P.O.Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("PO Box."), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("PO Bx"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P.o. Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("POBox"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("BO Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("p.o. Box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("P.O. Box."), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("Po box"), stringr::fixed("P.O. Box")),
    Address_old = str_replace(Address_old, stringr::fixed("PX Box"), stringr::fixed("P.O. Box")),
    Address_matching = Address_old,
    Notes = if_else(is.na(Notes), "", Notes),
    `Enhancement info` = if_else(is.na(`Enhancement info`), "", `Enhancement info`),
    organizer_old = if_else(is.na(organizer_old), "", organizer_old),
  ) %>% 
  # adjust PO boxes to resolve errors in numbers
  mutate(
    Address_matching = case_when(
      Address_matching == "P.O. Box 2670" ~ "P.O. Box 2760",
      `Prison Abbreviation` == "CCC" ~ "P.O. Box 2210",
      Address_matching == "P.O. Box 1406" &  `Prison Abbreviation` == "CCI" ~ "P.O. Box 1906",
      `Prison Abbreviation` == "CCTRP-Santa Fe Springs" ~ "P.O. Box 3205",
      `Prison Abbreviation` == "CCTRP-San Diego" ~ "3050 Armstrong St.",
      `Prison Abbreviation` == "CCTRP-Bakersfield" ~ "1932 Jessie St.",
      `Prison Abbreviation` == "CCTRP-Stockton" ~ "1609 N. Wilson Way",
      `Prison Abbreviation` == "CMC" ~ "P.O. Box 8103",
      Address_matching == "P.O. Box 1501" &  `Prison Abbreviation` == "CCWF" ~ "P.O. Box 1508",
      Address_matching == "P.O. Box 8931" &  `Prison Abbreviation` == "CEN" ~ "P.O. Box 931",
      Address_matching == "P.O. Box 213040," &  `Prison Abbreviation` == "CHCF" ~ "P.O. Box 213040",
      Address_matching == "P.O. Box 213046" &  `Prison Abbreviation` == "CHCF" ~ "P.O. Box 213040",
      Address_matching == "P.O. Box 21340" &  `Prison Abbreviation` == "CHCF" ~ "P.O. Box 213040",
      Address_matching == "P.O. Box 32110" &  `Prison Abbreviation` == "CHCF" ~ "P.O. Box 213040",
      Address_matching == "P.O. Box 0600" &  `Prison Abbreviation` == "CIM" ~ "P.O. Box 600",
      Address_matching == "P.O. Box 6000" &  `Prison Abbreviation` == "CIM" ~ "P.O. Box 600",
      Address_matching == "P.o.Box 500" &  `Prison Abbreviation` == "CIM" ~ "P.O. Box 600",
      stringr::str_detect(Address_matching, "16756 Chino|16765 Chino") == TRUE ~ "16756 Chino-Corona Rd.",
      `Prison Abbreviation` == "CMC-East" ~ "P.O. Box 8103",
      `Prison Abbreviation` == "CMC-West" ~ "P.O. Box 8103",
      `Prison Abbreviation` == "CMF" ~ "P.O. Box 2000",
      `Prison Abbreviation` == "CRC" ~ "P.O. Box 3535",
      Address_matching == "P.O. Box 1050" &  `Prison Abbreviation` == "CTF" ~ "P.O. Box 686",
      Address_matching == "P.O. Box 704" &  `Prison Abbreviation` == "CTF" ~ "P.O. Box 705",
      Address_matching == "P.O. Box 715071" &  `Prison Abbreviation` == "CIM" ~ "P.O. Box 705",
      `Prison Abbreviation` == "CTF-Central" ~ "P.O. Box 689",
      `Prison Abbreviation` == "CVSP" ~ "P.O. Box 2349",
      `Prison Abbreviation` == "Fire Camp-Washington Ridge #44" ~ "11425 Conservation Camp Rd.",
      `Prison Abbreviation` == "FSP" ~ "P.O. Box 715071",
      `Prison Abbreviation` == "HDSP" ~ "P.O. Box 3030",
      `Prison Abbreviation` == "ISP" ~ "P.O. Box 2199",
      stringr::str_detect(Address_matching, "P.O. Box 3031|P.O. Box 5000|P.O. Box 6000") &  `Prison Abbreviation` == "KVSP" ~ "P.O. Box 3130",
      is.na(Address_matching) & `Prison Abbreviation` == "KVSP" ~ "P.O. Box 3130",
      Address_matching == "P.O. Box 5704" &  `Prison Abbreviation` == "KVSP" ~ "P.O. Box 5104",
      Address_matching == "P.O. Box 5001" &  `Prison Abbreviation` == "KVSP" ~ "P.O. Box 5101",
      Address_matching == "P.O. Box 4300" &  `Prison Abbreviation` == "LAC" ~ "P.O. Box 8457",
      is.na(Address_matching) &  `Prison Abbreviation` == "LAC" ~ "P.O. Box 8457",
      Address_matching == "P.O. Box 5101" &  `Prison Abbreviation` == "NKSP" ~ "P.O. Box 5000",
      Address_matching == "P.O. Box 5104" &  `Prison Abbreviation` == "NKSP" ~ "P.O. Box 5000",
      `Prison Abbreviation` == "RJD" ~ "480 Alta Rd.",
      `Prison Abbreviation` == "PBSP" ~ "P.O. Box 7500",
      `Prison Abbreviation` == "PVSP" ~ "P.O. Box 8500",
      `Prison Abbreviation` == "SAC" ~ "P.O. Box 290001",
      Address_matching == "PO 5242" &  `Prison Abbreviation` == "SATF" ~ "P.O. Box 5242",
      Address_matching == "PO 5244" &  `Prison Abbreviation` == "SATF" ~ "P.O. Box 5244",
      Address_matching == "P.O. Box 3481" &  `Prison Abbreviation` == "SATF" ~ "P.O. Box 7100",
      `Prison Abbreviation` == "SCC" ~ "5150 O’Byrnes Ferry Rd.",
      `Prison Abbreviation` == "SSC" ~ "5150 O’Byrnes Ferry Rd.",
      `Prison Abbreviation` == "SOL" ~ "P.O. Box 4000",
      `Prison Abbreviation` == "SVSP" ~ "P.O. Box 1050",
      `Prison Abbreviation` == "VSP" ~ "P.O. Box 92",
      TRUE ~ Address_matching
    ),
    `Prison Abbreviation` = case_when(
      Address_matching == "P.O. Box 2210" &  `Prison Abbreviation` == "CCI" ~ "CCC",
      Address_matching == "P.O. Box 2760" &  `Prison Abbreviation` == "CCI" ~ "CAC",
      Address_matching == "P.O. Box 921" &  `Prison Abbreviation` == "CCI" ~ "CEN",
      `Prison Abbreviation` == "CCWP" ~ "CCWF",
      `Prison Abbreviation` == "CFCF" ~ "CHCF",
      `Prison Abbreviation` == "MSCP" ~ "MCSP",
      `Prison Abbreviation` == "SSC" ~ "SCC",
      Address_matching == "P.O. Box 5244" &  `Prison Abbreviation` == "CSP" ~ "SATF",
      Address_matching == "33015 Bautista Rd." ~ "Fire Camp-Bautista #36",
      Address_matching == "Fire Camp-Ishi" ~ "Fire Camp-Ishi CC #18",
      Address_matching == "Fire Camp - Antelope" ~ "Fire Camp - Antelope CC#25",
      Address_matching == "P.O. Box 8800" &  `Prison Abbreviation` == "SATF" ~ "COR",
      TRUE ~ `Prison Abbreviation`
    )
  ) %>%
  # generate columns related to prison information
  dplyr::left_join(prison, by = c("Prison Abbreviation", "Address_matching" = "PO Box or Address")) %>% 
  # generate the columns with the most important/relevant information for those inside
  mutate(
    `Three Strikes` = dplyr::if_else(stringr::str_detect(Notes, "Three strikes|three strikes|3 strikes|Three Strikes|three Strikes|3 Strikes"), TRUE, FALSE),
    `Life Without Parole ( LWOP)` = dplyr::case_when(
      stringr::str_detect(Notes, "LWOP|lwop|Lwop") == TRUE ~ TRUE,
      stringr::str_detect(`Enhancement info`, "LWOP|lwop|Lwop") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Death Penalty` = dplyr::case_when(
      stringr::str_detect(Notes, "Death Penalty|death penalty|Death penalty|Death Row|death row|Death row") == TRUE ~ TRUE,
      stringr::str_detect(`Enhancement info`, "Death Penalty|death penalty|Death penalty|Death Row|death row|Death row") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Great Bodily Injury` = dplyr::if_else(stringr::str_detect(`Enhancement info`, "GBI|gbi|Gbi"), TRUE, FALSE),
    `Prison Prior (1 year)` = dplyr::case_when(
      stringr::str_detect(Notes, "Prison Prior|Prison prior|prison prior") == TRUE ~ TRUE,
      stringr::str_detect(`Enhancement info`, "Prison Prior|Prison prior|prison prior") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Prior Felony (5 year)` = dplyr::case_when(
      stringr::str_detect(Notes, "Prior Felony|Prior felony|prior felony") == TRUE ~ TRUE,
      stringr::str_detect(`Enhancement info`, "Prior Felony|Prior felony|prior felony") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    enhance_less_gang = str_replace_all(`Enhancement info`, c("gang" = "", "GBI" = "", "drugs" = "")),
    `Gun / Firearm` = dplyr::case_when(
      stringr::str_detect(enhance_less_gang, "g") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Gang` = dplyr::case_when(
      stringr::str_detect(`Enhancement info`, "Gang|gang") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Drug (3 year)` = dplyr::case_when(
      stringr::str_detect(`Enhancement info`, "drug|Drug") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `3 strikes` = `Three Strikes`, # duplicate column
    `Youth Parole` = dplyr::if_else(stringr::str_detect(Notes, "Youth Parole|Youth parole|youth parole"), TRUE, FALSE),
    `Elder Parole` = dplyr::if_else(stringr::str_detect(Notes, "Elder Parole|Elder parole|elder parole|elderly|Elderly"), TRUE, FALSE),
    `Prop 57` = dplyr::if_else(stringr::str_detect(Notes, "Proposition 57|Prop 57|prop 57|Prop57|prop57"), TRUE, FALSE),
    `Medical Parole` = dplyr::if_else(stringr::str_detect(Notes, "Medical Parole|Medical parole|medical parole"), TRUE, FALSE),
    `Has 1170 Referral` = dplyr::if_else(stringr::str_detect(Notes, "1170"), TRUE, FALSE),
    `Inside Organizer` = dplyr::case_when(
      is.na(organizer_old) ~ FALSE,
      stringr::str_detect(`Enhancement info`, "x") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ), # mark as an organizer if that column has text in it
    `Willing to Speak to Media` = FALSE,
    `Jailhouse Lawyer` = FALSE,
    `Applied for Commutation` = dplyr::if_else(stringr::str_detect(Notes, "Commutation|commutation|Applied|applied|Clemency|clemency"), TRUE, FALSE),
    `Commuted` = dplyr::if_else(stringr::str_detect(Notes, "Commuted|commuted"), TRUE, FALSE),
    # note that medical reprieve/release is likely to catch those that have applied but not received it
    `Received a Medical Reprieve` = dplyr::if_else(stringr::str_detect(Notes, "Medical reprieve|medical repriece|Medical release|medical release"), TRUE, FALSE),
    `Innocence Claim` = dplyr::if_else(stringr::str_detect(Notes, "Innocence|innocence|Wrongful|wrongful"), TRUE, FALSE),
    `Fire Camp` = dplyr::case_when(
      stringr::str_detect(Notes, "Fire camp|fire camp") == TRUE ~ TRUE,
      stringr::str_detect(`Prison Abbreviation`, "Fire camp|fire camp") == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    `Stamps Campaign Member` = dplyr::if_else(stringr::str_detect(Notes, "Stamps|stamps"), TRUE, FALSE),
    `LGBTQ` = dplyr::if_else(stringr::str_detect(Notes, "LGBT|lgbt|Transgender|transgender|Non-binary|non-binary|nonbinary|Nonbinary|pronouns|trans woman|trans man"), TRUE, FALSE),
    `Transgender, Nonbinary & Gender Nonconfo` = dplyr::if_else(stringr::str_detect(Notes, "Transgender|transgender|Non-binary|non-binary|nonbinary|Nonbinary|pronouns|trans woman|trans man"), TRUE, FALSE),
    `Spanish language` = dplyr::if_else(stringr::str_detect(Notes, "Spanish|spanish"), TRUE, FALSE),
    `Received a Toolkit` = dplyr::if_else(stringr::str_detect(Notes, "Toolkit|toolkit|TK|tk|Tool kit|tool kit"), TRUE, FALSE),
    `Current Inside Fellow` = FALSE,
    `Former Inside Fellow` = FALSE,
    `County of Conviction - Current Case` = dplyr::case_when(
      stringr::str_detect(Notes, "Alameda|alameda") == TRUE ~ "Alameda",
      stringr::str_detect(Notes, "LA county|LA County") == TRUE ~ "LA County",
      TRUE ~ "Unknown"
    ),
  ) %>% 
  select(-enhance_less_gang) %>% # remove temporary column 
  # generate early columns where the values are default the same for each person
  mutate(
    Phone = "Unknown",
    Email = "Unknown",
    `Act-On Behavior Score` = "Unknown",
    `County of Conviction - Priors` = "Unknown",
    `Address Type` = "Unknown",
    `Contact Type` = "Currently Incarcerated",
    `Archived` = FALSE,
    `Date Archived` = FALSE,
    `Deceased` = FALSE,
    Solicitor = FALSE, # Not sure what Solicitor means?
    `Currently Incarcerated` = TRUE,
    Pretrial = FALSE, # Should we default false here? 
    Determinate = NA_character_, 
    Indeterminate = NA_character_
  ) %>% 
  # generate later columns where details are unknown
  dplyr::mutate(
    `Personal Relationship to EBC Team or All` = NA_character_,
    `Personal Relationship to EBC Team/Ally` = NA_character_,
    `Met EBC Team in Person` = NA_character_,
    `Met EBC Team in Person Event` = NA_character_,
    `NOSL Artist` = NA_character_,
    `Zine Artist` = NA_character_, 
    `Additional Needs` = NA_character_,
    `Occupation` = NA_character_,
    `Title` = NA_character_,
    `Age Range` = NA_character_,
    `Ethnicity` = NA_character_,
    `Ethnicity Other` = NA_character_,
    `Primary Language` = NA_character_,
    `Gender` = NA_character_,
    `Greeting` = NA_character_,
    `Birthdate` = NA_character_,
    `Website` = NA_character_,
    `Blog` = NA_character_,
    `Do Not Call` = NA_character_,
    `Mobile` = NA_character_,
    `Home Phone` = NA_character_,
    `Work Phone` = NA_character_,
    `Work Email` = NA_character_,
    `Email Opt Out` = NA_character_,
    `Phone Opt-Out` = NA_character_,
    `Text Opt Out` = NA_character_,
    `Restore Oakland Visit` = NA_character_,
    `Date of Visit` = NA_character_,
    `RO Visit Summary` = NA_character_,
    `Type of Site Visit` = NA_character_,
    `Event Name` = NA_character_,
    `Invalid Address` = NA_character_,
    `Solicit via other methods` = NA_character_,
    `New Address - Research` = NA_character_,
    `Postal Mail Opt Out` = NA_character_,
    `Always Gives Anonymously` = NA_character_,
    `Major Donor Capacity` = NA_character_,
    `Capacity` = NA_character_,
    `Inclination` = NA_character_,
    `Readiness` = NA_character_,
    `Target Ask Amount` = NA_character_,
    `Engage Supporter Id` = NA_character_,
    `Email Subscription Status` = NA_character_,
    `Salsa Last Update` = NA_character_,
    `Engage External Id` = NA_character_,
    `Family of Incarcerated Person` = NA_character_,
    `Family of Law Enforcement Violence Survi`= NA_character_,
    `Formerly Incarcerated Person` = NA_character_,
    `Law Enforcement Violence Survivor` = NA_character_,
    `Member Skills` = NA_character_,
    `Membership Join Date` = NA_character_,
    `Membership End Date` = NA_character_,
    `Engagement Level` = NA_character_,
    `RO Unique Identifier` = NA_character_,
    `RO First Name` = NA_character_,
    `RO Last Name` = NA_character_,
    `RO Nickname/Preferred Name` = NA_character_,
    `RO Spouse/Partner First Name` = NA_character_,
    `RO Spouse/Partner Last Name` = NA_character_,
    `RO Spouse/Partner Nickname` = NA_character_,
    `RO Do Not Email` = NA_character_,
    `RO Email` = NA_character_,
    `RO Alternate Email` = NA_character_,
    `RO Do Not Call` = NA_character_,
    `RO Home Phone Number` = NA_character_,
    `RO Cell Phone Number` = NA_character_,
    `RO Street Address` = NA_character_,
    `RO City` = NA_character_,
    `RO State` = NA_character_,
    `RO Zip` = NA_character_,
    `RO Total Gifts` = NA_character_,
    `RO Gift Amounts & Dates` = NA_character_,
    `RO Campaign Notes` = NA_character_,
    `RO Biographic Notes` = NA_character_   
  ) %>% 
  dplyr::select(`First Name`:Notes, `County of Conviction - Current Case`, `County of Conviction - Priors`, `Youth Parole`, `Elder Parole`, `Has 1170 Referral`, `Applied for Commutation`, dplyr::everything())
```

    ## Warning: Coercing `pattern` to a plain character vector.

    ## Warning: Coercing `pattern` to a plain character vector.

``` r
small_ebc <- 
  clean_ebc %>% 
  head(20)

# write this datatable to google sheets
# write_sheet(clean_ebc, ss = "https://docs.google.com/spreadsheets/d/1j6iL0DnYdvrS8M9EJN3V772Eq1phmogLD1ZEWuxPtTs/edit#gid=0", sheet = "From Analysis 8-17")
```

## Identify prisons that are incorrectly labelled

``` r
# 
# prison_unlinked2 <-
#   clean_ebc %>%
#   mutate(row_num = row_number()) %>%
#   dplyr::filter(is.na(`Prison Name`)) %>%
#   dplyr::count(`Prison Abbreviation`, Address_matching) %>%
#   arrange(`Prison Abbreviation`)
# 
# prison_unlinked <-
#   clean_ebc %>%
#   mutate(row_num = row_number()) %>%
#   dplyr::filter(is.na(`Prison Name`)) %>%
#   dplyr::distinct(`Prison Abbreviation`, Address_matching, .keep_all = TRUE) %>%
#   dplyr::select(`Prison Abbreviation`, Address_matching, `Prison ID Number`) %>%
#   arrange(`Prison Abbreviation`)

# write_sheet(prison_unlinked_2, ss = "https://docs.google.com/spreadsheets/d/1j6iL0DnYdvrS8M9EJN3V772Eq1phmogLD1ZEWuxPtTs/edit#gid=0", sheet = "Prison_PO_Box_fix")
```
