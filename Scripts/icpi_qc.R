
df_filepath<-"~/Github/ICPIqc/data/test_MER_Structured_Datasets_SITE_IM_FY19-21_20210813_Eswatini.txt"


df<-read_msd(df_filepath)


##################Indicator/disagg completeness

#ADD 2021 Q3 TO DEFAULT SELECTION for third table
df %>% 
  filter(fiscal_year=="2021",
         standardizeddisaggregate=="Total Numerator",
         indicator=="TX_CURR",
         qtr3>0) %>% 
  distinct(facilityuid) %>% 
  count()


df %>% 
  filter(fiscal_year=="2021",
         indicator=="SC_ARVDISP",
         qtr2>0) %>% 
  distinct(facilityuid) %>% 
  count()

df %>% 
  filter(fiscal_year=="2021",
         standardizeddisaggregate=="Age/Sex/ARVDispense/HIVStatus",
         indicator=="TX_CURR",
         qtr3>0) %>% 
  distinct(facilityuid) %>% 
  count()

df %>% 
  filter(fiscal_year=="2021",
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator"),
         indicator=="TX_CURR") %>% 
  group_by(standardizeddisaggregate) %>% 
  summarise(across(c(qtr3), sum, na.rm = TRUE))

###################Supply chain at a glance

df %>%
  filter(fiscal_year==2021,
         indicator %in% c("SC_CURR", "SC_ARVDISP")) %>% 
  group_by(indicator, otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()
#PEDIATRIC MEDS SAME COLOR AS TLD


df %>%
  filter(fiscal_year==2021,
         indicator %in% c("SC_CURR", "SC_ARVDISP"),
         str_detect(otherdisaggregate, "TLD")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                  "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                         otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(fiscal_year==2021,
         indicator=="SC_CURR",
         str_detect(otherdisaggregate, "TLD")) %>% 
  distinct(facilityuid)

df %>% 
  filter(fiscal_year==2021,
         indicator=="SC_CURR",
         qtr2>0,
         str_detect(otherdisaggregate, "TLE")) %>%
  distinct(facilityuid)


df %>% 
  filter(fiscal_year==2021,
         indicator=="SC_CURR",
         qtr2>0,
         otherdisaggregate %in% c("ARV Bottles - TLE/400 90-count", "ARV Bottles - TLE/400 30-count")) %>%
  distinct(orgunituid)

df %>% 
  filter(fiscal_year==2021,
         indicator=="SC_CURR",
         qtr2>0,
         otherdisaggregate %in% c("ARV Bottles - NVP Adult")) %>%
  distinct(orgunituid)

df %>%
  filter(fiscal_year==2021,
         indicator %in% c("SC_CURR")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator, otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()

df %>% 
  filter(fiscal_year==2021,
         indicator=="TX_CURR",
         standardizeddisaggregate=="Age/Sex/ARVDispense/HIVStatus") %>% 
  group_by(otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm=TRUE))

df %>% 
  filter(fiscal_year==2021,
         indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator")) %>% 
  group_by(otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm=TRUE))


################Pediatrics at a glance

#SHOULDN'T BE A 2021 Q4 OPTION
df %>%
  filter(indicator %in% c("SC_CURR", "SC_ARVDISP")) %>% 
  group_by(indicator, otherdisaggregate, fiscal_year) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()

##############Stock levels
#FILTER SHOULD HAVE 2021 Q3/Q4


##############Stock Map
df %>%
  filter(indicator %in% c("SC_CURR"),
         fiscal_year==2021,
         facility=="The Luke Commission") %>% 
  group_by(indicator, otherdisaggregate, fiscal_year) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()

##############ARVs Dispensed


df %>%
  filter(fiscal_year==2021,
         indicator %in% c("SC_ARVDISP")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator, otherdisaggregate) %>% 
  summarise(across(c(qtr2), sum, na.rm = TRUE)) %>% 
  View()



##############Dispensed vs Expected

df %>%
  filter(fiscal_year==2021,
         indicator %in% c("SC_ARVDISP")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  summarise(across(c(qtr2), sum, na.rm = TRUE))

df %>%
  filter(fiscal_year==2021,
         indicator %in% c("TX_CURR"),
         standardizeddisaggregate=="Total Numerator") %>% 
  summarise(across(c(qtr1, qtr2), sum, na.rm = TRUE))


##############TLD Transition

df %>%
  filter(indicator %in% c("SC_CURR", "SC_ARVDISP"),
         str_detect(otherdisaggregate, "TLD")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator, fiscal_year) %>% 
  summarise(across(c(qtr2, qtr4), sum, na.rm = TRUE)) %>% 
  View()

df %>%
  filter(indicator %in% c("SC_CURR", "SC_ARVDISP"),
         str_detect(otherdisaggregate, "TLD")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator, fiscal_year, otherdisaggregate) %>% 
  summarise(across(c(qtr2, qtr4), sum, na.rm = TRUE)) %>% 
  View()


############## MMD vs Bottle Count
df %>%
  filter(indicator %in% c("SC_CURR", "SC_ARVDISP"),
         str_detect(otherdisaggregate, "ARV Bottles - TLD 90-count")) %>% 
  mutate(qtr2 = case_when(otherdisaggregate %in% c("ARV Bottles - TLD 90-count",
                                                   "ARV Bottles - TLE/400 90-count") ~ (qtr2*3),
                          otherdisaggregate=="ARV Bottles - TLD 180-count" ~ (qtr2*6), TRUE ~ qtr2)) %>%
  group_by(indicator, fiscal_year) %>% 
  summarise(across(c(qtr2, qtr4), sum, na.rm = TRUE)) %>% 
  View()

############## MMD Trends

##DEFAULT Q3

df %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator")) %>% 
  group_by(otherdisaggregate, fiscal_year) %>% 
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE))


df %>% 
  filter(indicator=="TX_CURR",
         facility=="Lamvelase Clinic",
         standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus", "Total Numerator")) %>% 
  group_by(otherdisaggregate, standardizeddisaggregate, fiscal_year) %>% 
  summarise(across(c(qtr1, qtr2, qtr3, qtr4), sum, na.rm=TRUE))

################# Viral Load Suppression

df %>% 
  filter(indicator=="TX_PVLS",
         facility=="Bulembu Clinic",
         fiscal_year=="2021") %>% 
  group_by(otherdisaggregate, standardizeddisaggregate, numeratordenom) %>% 
  summarise(across(c(qtr2), sum, na.rm=TRUE))
