# -------------------------------
# DISERTASI: EPE & GHG EMISSIONS
# -------------------------------
# Judul: Can Government Environmental Spending Reduce Emissions?
# Periode: 46 Countries, 2008–2021
# -------------------------------

# 1. PERSIAPAN
# install.packages(c("plm", "dplyr", "ggplot2", "readr", "car", "stargazer", "lmtest"))
library(plm)
library(dplyr)
library(ggplot2)
library(readr)
library(car)
library(stargazer)
library(lmtest)
library(readxl)

# 2. BACA DATA
data <- read_excel("Dataset Original.xlsx")
str(data)

# 3. DEFINISI DATA PANEL
data <- data %>%
  mutate(Country = as.factor(ISO3),
         Year = as.integer(Year))

pdata <- pdata.frame(data, index = c("Country", "Year"))

# 4. PEMBUATAN VARIABEL MODEL
pdata$EPE_lag <- lag(pdata$Environmental.Protection.Expenditure)
pdata$EPE_lag_sq <- pdata$EPE_lag^2

# Institutional Quality sebagai rata-rata 6 indikator
pdata$IQ <- rowMeans(pdata[, c(
  "Control.of.Corruption..Percentile.Rank..CC.PER.RNK.",
  "Government.Effectiveness..Percentile.Rank..GE.PER.RNK.",
  "Political.Stability.and.Absence.of.Violence.Terrorism..Percentile.Rank..PV.PER.RNK.",
  "Regulatory.Quality..Percentile.Rank..RQ.PER.RNK.",
  "Rule.of.Law..Percentile.Rank..RL.PER.RNK.",
  "Voice.and.Accountability..Percentile.Rank..VA.PER.RNK."
)], na.rm = TRUE)

# Interaction term
pdata$EPE_IQ <- pdata$EPE_lag * pdata$IQ

# 5. MODEL REGRESI PANEL

# Model Linear (H1, H2, H3)
model1 <- plm(GHG.Intensity ~ EPE_lag + IQ + EPE_IQ +
                GDP_Per_Capita_PPP + FDI + Electricity_Consumption_per_Capita +
                Urban_Population_Percent + Renewable_Energy_Consumption +
                Agriculture.Land,
              data = pdata, model = "within", effect = "twoways")

summary(model1)

# Model Non-Linear (H4)
model2 <- plm(GHG.Intensity ~ EPE_lag + EPE_lag_sq + IQ + EPE_IQ +
                GDP_Per_Capita_PPP + FDI + Electricity_Consumption_per_Capita +
                Urban_Population_Percent + Renewable_Energy_Consumption +
                Agriculture.Land,
              data = pdata, model = "within", effect = "twoways")

summary(model2)

# 6. HAUSMAN TEST: FIXED VS RANDOM
re_model <- plm(GHG.Intensity ~ EPE_lag + IQ + EPE_IQ +
                  GDP_Per_Capita_PPP + FDI + Electricity_Consumption_per_Capita +
                  Urban_Population_Percent + Renewable_Energy_Consumption +
                  Agriculture.Land,
                data = pdata, model = "random")

hausman_test <- phtest(model1, re_model)
print(hausman_test)

# 8. MULTIKOLINEARITAS (VIF)
ols_model <- lm(GHG.Intensity ~ Environmental.Protection.Expenditure + IQ +
                  GDP_Per_Capita_PPP + FDI + Electricity_Consumption_per_Capita +
                  Urban_Population_Percent + Renewable_Energy_Consumption +
                  Agriculture.Land, data = pdata)

vif(ols_model)

# 9. OUTPUT TABEL HASIL
stargazer(model1, model2, type = "text",
          title = "Panel Regression Results",
          column.labels = c("Linear", "Non-Linear"),
          model.numbers = TRUE)

# 10. PSM CHECK
#install.packages("MatchIt")
library(MatchIt)

# Median split atau tertile (bisa disesuaikan)
pdata$EPE_treat <- ifelse(pdata$EPE_lag > median(pdata$EPE_lag, na.rm = TRUE), 1, 0)

# Drop NA untuk variabel penting
psm_data <- pdata %>%
  filter(!is.na(EPE_treat), !is.na(GHG.Intensity), !is.na(IQ)) %>%
  select(EPE_treat, GHG.Intensity, IQ,
         GDP_Per_Capita_PPP, FDI, Electricity_Consumption_per_Capita,
         Urban_Population_Percent, Renewable_Energy_Consumption, Agriculture.Land)

# Matching menggunakan nearest neighbor
psm_model <- matchit(EPE_treat ~ IQ + GDP_Per_Capita_PPP + FDI +
                       Electricity_Consumption_per_Capita +
                       Urban_Population_Percent + Renewable_Energy_Consumption +
                       Agriculture.Land,
                     data = psm_data, method = "nearest", ratio = 1)

summary(psm_model)

plot(psm_model, type = "jitter")