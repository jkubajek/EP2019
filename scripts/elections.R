# ###################################################################
# Libraries
# ###################################################################
# https://robertmitchellv.com/blog-building-site-with-rmarkdown.html
library(dplyr)
library(tidyr)
library(foreign)
library(openxlsx)
library(ggplot2)

library(rgdal)
library(sp) # do tworzenia centroidow
library(maptools)
library(rgeos)
library(RColorBrewer)
library(ggmap)

library(geospacom)
library(spdep)

library(grid)

library(leaflet)
library(widgetframe)
library(stargazer)
# ###################################################################
# Setting working directory and loading data
# ###################################################################
if(Sys.info()["sysname"] == "Windows"){
    main_dir <- "D:/Osobiste/GitHub"
} else {
    main_dir <- "/home/rstudio/"
}
setwd(main_dir)

source("EP2019/scripts/functions.R", encoding = "UTF-8")

options(stringsAsFactors = F)
votes <- read.xlsx("EP2019/data/wyniki_gl_na_listy_po_powiatach.xlsx") %>%
    dplyr::select(TERYT, nazwa, votes_all, votes_ke, votes_pis, votes_wiosna, 
                  votes_konfederacja, votes_kukiz, votes_razem) %>%
    mutate(votes_pis_perc = (votes_pis / votes_all) * 100,
           votes_wiosna_perc = (votes_wiosna / votes_all) * 100,
           votes_ke_perc = (votes_ke / votes_all) * 100 ,
           votes_konfederacja_perc = (votes_konfederacja / votes_all) * 100,
           votes_kukiz_perc = (votes_kukiz / votes_all) * 100,
           votes_razem_perc = (votes_razem / votes_all) * 100,
           TERYT = substr(TERYT, 1, 4)) %>%
    dplyr::select(TERYT, nazwa, votes_pis_perc, votes_wiosna_perc, votes_ke_perc,
                  votes_konfederacja_perc, votes_kukiz_perc, votes_razem_perc)

proc_data <- read.xlsx("EP2019/data/wyniki_gl_na_listy_po_powiatach_proc.xlsx") %>%
    dplyr::select(TERYT, frekwencja, proc_niewaznych) %>%
    mutate(frekwencja = gsub(frekwencja, pattern = ",", replacement = ".", fixed = T) %>% as.numeric(),
           proc_niewaznych = gsub(proc_niewaznych, pattern = ",", replacement = ".", fixed = T) %>% as.numeric(),
           TERYT = substr(TERYT, 1, 4))

votes_2015 <- read.xlsx("EP2019/data/2015-gl-lis-pow.xlsx") %>%
    dplyr::select(TERYT, votes_2015_all, votes_2015_po, votes_2015_pis, 
                  votes_2015_psl, votes_2015_sld, votes_2015_n,
                  votes_2015_korwin, votes_2015_braun, votes_2015_kukiz, votes_2015_razem,
                  wydane_karty, l_wyborcow) %>%
    mutate(votes_2015_pis_perc = (votes_2015_pis / votes_2015_all) * 100,
           votes_2015_ke_perc = ((votes_2015_po + votes_2015_sld + votes_2015_psl + 
                                     votes_2015_n) / votes_2015_all) * 100,
           votes_2015_braun = ifelse(is.na(votes_2015_braun), 0, votes_2015_braun),
           votes_2015_konfederacja_perc = ((votes_2015_korwin + votes_2015_braun) / votes_2015_all) * 100,
           votes_2015_kukiz_perc = (votes_2015_kukiz /votes_2015_all) * 100,
           votes_2015_razem_perc = (votes_2015_razem /votes_2015_all) * 100) %>%
    left_join(read.xlsx("EP2019/data/2015-gl-lis-pow-proc.xlsx") %>%
                  dplyr::select(TERYT, Frekwencja) %>%
                  rename(frekwencja_2015 = Frekwencja), by = "TERYT") %>%
    dplyr::select(TERYT, votes_2015_pis_perc, votes_2015_ke_perc, frekwencja_2015, votes_2015_konfederacja_perc,
                  votes_2015_kukiz_perc, votes_2015_razem_perc)

salaries <- read.xlsx("EP2019/data/WYNA_2497_XTAB_20190527184602.xlsx", sheet = "TABLICA") %>%
    dplyr::select(Kod, X18) %>%
    rename(TERYT = Kod,
           salary = X18) %>%
    .[-c(1, 2), ] %>%
    mutate(salary = salary %>% as.numeric(),
           salary_log = salary %>% log(),
           TERYT = substr(TERYT, 1, 4))

unempl <- read.xlsx("EP2019/data/RYNE_3559_XTAB_20190531201918.xlsx", sheet = "TABLICA") %>%
    rename(TERYT = Kod) %>%
    mutate(unempl_diff = unempl_2015_04 - unempl_2019_04,
           unempl_factor = unempl_diff / unempl_2015_04 * 100,
           unempl_2019_04_log = unempl_2019_04 %>% log(),
           unempl_factor_log = unempl_factor %>% log(),
           TERYT = substr(TERYT, 1, 4)) %>%
    dplyr::select(TERYT, unempl_2015_04, unempl_2019_04, unempl_diff, unempl_factor, 
                  unempl_2019_04_log, unempl_factor_log)

pop_density <- read.xlsx("EP2019/data/LUDN_2425_XTAB_20190527210859.xlsx", sheet = "TABLICA") %>%
    dplyr::select(Kod, ludnosc.na.1.km2) %>%
    rename(TERYT = Kod,
           pop_density = ludnosc.na.1.km2) %>%
    .[-c(1, 2), ] %>%
    mutate(pop_density = pop_density %>% as.numeric(),
           pop_density_log = pop_density %>% log(),
           TERYT = substr(TERYT, 1, 4))

elderly <- read.xlsx("EP2019/data/LUDN_2137_XTAB_20190531194617.xlsx", sheet = "TABLICA") %>%
    left_join(read.xlsx("EP2019/data/LUDN_2137_XTAB_20190531200133.xlsx", sheet = "TABLICA"), by = "Kod") %>%
    mutate(elderly_perc = (age_60_64_2018 + age_65_69_2018 + age_70_plus_2018) / population_2018) %>%
    rename(TERYT = Kod) %>%
    mutate(TERYT = substr(TERYT, 1, 4),
           elderly_perc = elderly_perc * 100) %>%
    dplyr::select(TERYT, elderly_perc)

# Wartosc wydanych swiadczen 500+ na osobe w powiecie w 2017 r.
benefit_500 <- read.xlsx("EP2019/data/FINA_3758_XTAB_20190528171535.xlsx", sheet = "TABLICA") %>%
    left_join(read.xlsx("EP2019/data/LUDN_2137_XTAB_20190531200133.xlsx", sheet = "TABLICA"), by = "Kod") %>%
    mutate(benefit_500_pp = swiadczenia_os_fiz / population_2017 / 100,
           benefit_500_pp_nom = benefit_500_pp * 100) %>%
    rename(TERYT = Kod) %>%
    mutate(TERYT = substr(TERYT, 1, 4)) %>%
    dplyr::select(TERYT, benefit_500_pp, benefit_500_pp_nom)

partners <- read.xlsx("EP2019/data/NARO_3422_XTAB_20190531200908.xlsx", sheet = "TABLICA") %>%
    mutate(partners_perc = families_partners_2011 / families_all_2011 * 100,
           marriage_perc = families_married_2011 / families_all_2011 * 100) %>%
    rename(TERYT = Kod) %>%
    mutate(TERYT = substr(TERYT, 1, 4)) %>%
    mutate(partners_perc_log = log(partners_perc)) %>%
    dplyr::select(TERYT, partners_perc, marriage_perc, partners_perc_log)

flood <- read.xlsx("EP2019/data/politycy_powodz.xlsx") %>%
    dplyr::select(-nazwa) %>%
    mutate(politics = ifelse(is.na(politics), 0, politics),
           flood = ifelse(is.na(flood), 0, flood),
           TERYT = substr(TERYT, 1, 4))

matura <- read.xlsx("EP2019/data/SZKO_3374_XTAB_20190531110101.xlsx", sheet = "TABLICA") %>%
    rename(TERYT = Kod) %>%
    mutate(TERYT = substr(TERYT, 1, 4)) %>%
    dplyr::select(TERYT, matura_pass_rate_professional_2017, matura_pass_rate_general_2017)

education <- read.xlsx("EP2019/data/NARO_3309_XTAB_20190531160909.xlsx", sheet = "TABLICA") %>%
    rename(TERYT = Kod) %>%
    mutate(TERYT = substr(TERYT, 1, 4),
           uneducated_perc = uneducated_2011 / population_over_13_2011 * 100,
           primary_perc = primary_education_2011 / population_over_13_2011 * 100,
           primary_uneducated_perc = uneducated_perc + primary_perc) %>%
    dplyr::select(TERYT, uneducated_perc, primary_perc, primary_uneducated_perc)
# ###################################################################
# Joining dataset
# ###################################################################
dataset <- votes %>%
    inner_join(benefit_500, by = "TERYT") %>%
    inner_join(votes_2015, by = "TERYT") %>%
    inner_join(salaries, by = "TERYT") %>%
    inner_join(unempl, by = "TERYT") %>%
    inner_join(pop_density, by = "TERYT") %>%
    inner_join(elderly, by = "TERYT") %>%
    inner_join(proc_data, by = "TERYT") %>%
    inner_join(partners, by = "TERYT") %>%
    inner_join(flood, by = "TERYT") %>%
    inner_join(matura, by = "TERYT") %>%
    inner_join(education, by = "TERYT") %>%
    mutate(TERYT = substr(TERYT, 1, 4),
           votes_pis_perc_diff = votes_pis_perc - votes_2015_pis_perc,
           votes_ke_perc_diff = votes_ke_perc - votes_2015_ke_perc,
           votes_konfederacja_perc_diff = votes_konfederacja_perc - votes_2015_konfederacja_perc,
           votes_kukiz_perc_diff = votes_kukiz_perc - votes_2015_kukiz_perc,
           votes_razem_perc_diff = votes_razem_perc - votes_2015_razem_perc,
           frekwencja_diff = frekwencja - frekwencja_2015,
           rand_val = rnorm(nrow(.))) %>%
    mutate(POW = substr(TERYT, 3, 4) %>% as.numeric(),
           miasto_powiatowe = ifelse(POW > 40, 1, 0),
           pop_density_log_miasta = pop_density_log * miasto_powiatowe,
           frekwencja_diff = frekwencja - frekwencja_2015) %>%
    as.data.frame()

save(dataset, file = "EP2019/data/dataset.RData")

# ###################################################################
# Initial analysis
# ###################################################################
model_frekwencja <- lm(frekwencja ~ unempl + 
                           salary_log + pop_density_log + 
                           #children_perc +
                           elderly_perc, data = dataset)
model_frekwencja %>% summary()

model_age <- lm(children_perc ~ elderly_perc, data = dataset)
model_age %>% summary()

model_benefit <- lm(benefit_500_pp ~ elderly_perc + primary_uneducated_perc, data = dataset)
model_benefit %>% summary()

model_rand <- lm(rand_val ~ elderly_perc + benefit_500_pp + salary_log +
                     unempl, data = dataset)
model_rand %>% summary()

# Plots of distribution
ggplot(dataset)+
    geom_density(aes(x = benefit_500_pp, y = ..density..))+
    theme_bw()

ggplot(dataset)+
    geom_density(aes(x = partners_perc_log, y = ..density..))+
    theme_bw()

ggplot(dataset)+
    geom_density(aes(x = salary_log, y = ..density..))+
    theme_bw()

ggplot(dataset)+
    geom_density(aes(x = log(unempl_2019_04), y = ..density..))+
    theme_bw()

# ###################################################################
# Unused R2 decomposition
# ###################################################################
# model_normalized <- lm(model_formula, data = normalize_DF(model_OLS$model))
# OLS_decomposed <- r2_decomposition(model_normalized)
# as.data.frame(OLS_decomposed)
# sum(unlist(OLS_decomposed), na.rm = T)

# ###################################################################
# Interactive map
# ###################################################################
load("EP2019/data/dataset.RData")
mapa <- readOGR(dsn = "Maps/Powiaty/powiaty.shp", layer = "powiaty")
mapa <- spTransform(mapa, "+proj=longlat")
# 
# # http://edrub.in/blog20170806.html
# mapa_2 <- rmapshaper::ms_simplify(mapa, keep = 0.01)
# mapa_2 <- spTransform(mapa_2, "+proj=longlat")
# par(mfrow = c(1, 2))
# plot(mapa_2)
# plot(mapa)
# 
# mapa <- mapa_2
# save(mapa, file = "EP2019/data/map_simplified.RData")
# load("EP2019/data/map_simplified.RData")
mapa <- merge(y = dataset, x = mapa, by.x = "jpt_kod_je", by.y = "TERYT")

# ###################################################################
# Econometric analysis
# ###################################################################
centroids <- coordinates(mapa)

# W matrix
w_mat <- (distm(centroids) / 10^3)
gamma <- 1
w_mat <- 1/(w_mat ^ gamma) * (w_mat <= 100)
diag(w_mat) <- 0
w_mat <- w_mat / rowSums(w_mat) # normalizacja macierzy W - dzielimy przez sumy wierszowe
w_mat <- mat2listw(w_mat) # Zamiana macierzy na liste - redukcja wymiarow

# Main Formula
model_formula <- votes_pis_perc ~
# model_formula <- votes_pis_perc_diff ~
# model_formula <- votes_konfederacja_perc ~
    unempl_2019_04_log +
    # unempl_2019_04 +
    # unempl_factor +
    salary_log +
    pop_density_log +
    # proc_niewaznych +
    frekwencja +
    flood +
    # matura_pass_rate_general_2017 +
    # uneducated_perc +
    # primary_perc +
    primary_uneducated_perc +
    # frekwencja_diff +
    partners_perc_log +
    benefit_500_pp +
    elderly_perc

formula_votes_2019 <- votes_pis_perc ~
    unempl_2019_04 +
    salary_log +
    pop_density_log +
    frekwencja +
    flood +
    primary_uneducated_perc +
    partners_perc_log +
    benefit_500_pp +
    elderly_perc

model_OLS <- lm(formula_votes_2019, data = mapa@data)
model_OLS %>% summary()
car::vif(model_OLS)

# R2 decomposition
relaimpo::calc.relimp(model_OLS)

# Wystepuja zaleznosci przestrzenne
moran_OLS <- lm.morantest(model_OLS, w_mat, alternative = "greater")


res <- model_OLS$residuals
colors <- brewer.pal(9, "BuGn") #set breaks for the 9 colors 
brks <- classIntervals(res, n=9, style="quantile")
brks <- brks$brks
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)

# Wykresy morana
moran.plot(res, w_mat, ylab="Opóźnienie przestrzenne reszt: W*e", xlab="Reszty: e", pch = 20, 
           main = "Wykres Morana sąsiedztwo I rzędu", col = sgh_green) 

# Model SAR Slag
model_SAR_Slag <- lagsarlm(formula_votes_2019, listw = w_mat, data = mapa@data)
model_SAR_Slag %>% summary()
moran_SAR <- moran.test(model_SAR_Slag$residuals, listw = w_mat)

# Model SEM
model_SEM <- errorsarlm(formula_votes_2019, listw = w_mat, data = mapa@data)
model_SEM %>% summary()
moran_SEM <- moran.test(model_SEM$residuals, listw = w_mat)


# Model SDEM
model_SDEM <- errorsarlm(formula_votes_2019, listw = w_mat, data = mapa@data, etype = "emixed")
model_SDEM %>% summary()
moran.test(model_SDEM$residuals, listw = w_mat)


# Model SARAR
model_SARAR <- sacsarlm(formula_votes_2019, listw = w_mat, data = mapa@data)
model_SARAR %>% summary()
moran_SARAR <- moran.test(model_SARAR$residuals, listw = w_mat)

model_SARAR$type <- model_SEM$type

save(model_OLS, moran_OLS, model_SAR_Slag, moran_SAR, model_SEM, moran_SEM,
     file = "Data/EP2019/models_PiS_2019.RData")

out <- stargazer::stargazer(model_OLS, model_SAR_Slag, model_SEM,
                            # type = 'html',
                            type = 'text',
                            dep.var.labels.include = FALSE,
                            dep.var.caption = "",
                            omit.stat = c("n"),
                            title = "Wyniki modeli",
                            single.row = TRUE, 
                            # column.labels = c("W - odległość", "W - I st.", "W - 5k"),
                            # digits = 2,
                            add.lines = list(c("Moran pvalue", return_moran_p_val(moran_OLS), return_moran_p_val(moran_SAR), 
                                               return_moran_p_val(moran_SEM)))
                            )

# Model on diff
formula_diff <- votes_pis_perc_diff ~
    unempl_2019_04 +
    salary_log +
    pop_density_log +
    frekwencja +
    flood +
    primary_uneducated_perc +
    partners_perc_log +
    benefit_500_pp +
    elderly_perc

model_OLS <- lm(formula_diff, data = mapa@data)
model_OLS %>% summary()
car::vif(model_OLS)

# Wystepuja zaleznosci przestrzenne
moran_OLS <- lm.morantest(model_OLS, w_mat, alternative = "greater")

# Model SAR Slag
model_SAR_Slag <- lagsarlm(formula_diff, listw = w_mat, data = mapa@data)
model_SAR_Slag %>% summary()
moran_SAR <- moran.test(model_SAR_Slag$residuals, listw = w_mat)

# Model SEM
model_SEM <- errorsarlm(formula_diff, listw = w_mat, data = mapa@data)
model_SEM %>% summary()
moran_SEM <- moran.test(model_SEM$residuals, listw = w_mat)

# Save models
save(model_OLS, moran_OLS, model_SAR_Slag, moran_SAR, model_SEM, moran_SEM,
     file = "Data/EP2019/models_diff_2019.RData")
##################################################
# Variables importance
##################################################
# https://www.displayr.com/how-is-variable-importance-calculated-for-a-random-forest/
variables_DF <- data.frame(variable = c("unempl_2019_04", "unempl_2019_04_log", "unempl_factor", "salary_log", 
                                        "pop_density_log", 
                                        "frekwencja", "primary_uneducated_perc",
                                        "partners_perc_log", "benefit_500_pp", "elderly_perc", 
                                        "flood"),
                           variable_name = c("Bezrobocie", "Logarytm bezrobocia", "Spadek bezrobocia", "Średnia płaca",
                                             "Gęstość zaludnienia", "Frekwencja 2019",
                                             "<= Podstawowe",
                                             "Partnerzy (NSP 2011)", "500+ na osobę", "Osoby 60+",
                                             "Powódź"))
# Random forest importance
# https://explained.ai/rf-importance/
RF_variables_importance(dataset, model_formula, variables_DF, 
                        plot_title = "Waga zmiennych w wyjaśnianiu poparcia PiS")

##################################################
# Analiza zmiennych - Korelacja miedzy zmiennymi
##################################################
my_fn <- function(data, mapping, method="loess", ...){
    p <- ggplot(data = data, mapping = mapping) + 
        geom_point() + 
        geom_smooth(method=method, ...)
    p
}

cor_data <- dataset %>%
    dplyr::select(votes_pis_perc, votes_pis_perc_diff, salary_log, unempl_2019_04, 
                  pop_density_log, elderly_perc, benefit_500_pp, partners_perc_log) %>%
    rename(pis_2019 = votes_pis_perc,
           pis_diff = votes_pis_perc_diff,
           unempl = unempl_2019_04,
           pop_dens_log = pop_density_log,
           partners_log = partners_perc_log) %>%
    data.frame()

GGally::ggpairs(cor_data, 
        upper = list(continuous = wrap("cor", size = 8),
                     discrete = wrap("facetbar", size = 6)),
        lower = list(continuous = wrap(my_fn, method="lm")))+
    theme_bw(base_size = 16)

ggsave("EP2019/plots/variables_correlation.png", width = 12, height = 12, dpi = 300)

# Site rendering
setwd(paste0(main_dir, "/EP2019"))
rmarkdown::render_site(encoding = "UTF-8")
