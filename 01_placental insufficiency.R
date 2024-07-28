
library(sf)
library(MASS)
library(lme4)
library(INLA)
library(dplyr)
library(moments)
library(splines)
library(data.table)
library(tidycensus)
library(performance)

case <- read.csv("/Users/brenna/Downloads/clean_case_data.csv")

# constructing the composite outcome > outcomes related to placental insufficiency
case$pl_insuf <- case_when(case$sga10_by_sex > case$birthweightgrams ~ 1, # sga
                           case$placenta_abr == 1 ~ 1)
case$pl_insuf <- ifelse(is.na(case$pl_insuf), 0, 1)

table(case$pl_insuf)


nu <- aggregate(case$pl_insuf, by = list(case$grid_id, case$week), FUN = sum)
denom <- case |>
  group_by(grid_id, week) |>
  count() |>
  rename(denom = n)
head(denom)

# all_st <- expand.grid(a$Group.1,
#                       a$Group.2)

grid_dat <- data.frame(grid_id = nu$Group.1,
                       week = nu$Group.2,
                       case = nu$x)
grid_dat <- merge(grid_dat, denom, by = c("grid_id", "week"), all.x = TRUE)
head(grid_dat)



slc_shp <- get_acs(geography = "county",
                   state = "UT",
                   variables = c("B03002_001"), # bach or higher 
                   geometry = TRUE,
                   year = 2021) |>
  filter(GEOID == "49035")

slc_shp <- st_transform(slc_shp, st_crs(grid))

grid <- st_read("/Users/brenna/Downloads/shapefiles") |>
  dplyr::select(grid_id)

# test <- st_join(dat_aq_shp, slc_shp, join = st_intersects) |>
#    filter(!is.na(GEOID))

case_sm <- case[, c("grid_id", "momid", "gestation", "week", "pl_insuf")]

case_sm$start_week <- case_sm$week - case_sm$gestation

case_sm <- case_sm |> 
  filter(start_week > 0) |>
  filter(momid %in% c(646037463, 616693064))

case_aq <- merge(aq, case_sm, by = "grid_id", all.x = FALSE)
case_aq <- case_aq |>
  filter(week.x >= start_week &
           week.y >= week.x) |>
  rename(birth_week = week.y,
         week = week.x)
write.csv(case_aq, "case-poll.csv", row.names = FALSE)

aq <- fread("/Users/brenna/Downloads/clean_dataset.csv")


aq <- aq |>
  filter(grid_id %in% grid_dat$grid_id)

dat_shp <- merge(grid, grid_dat, by = c("grid_id"), all.y = TRUE)

dat_aq_shp <- merge(grid_dat,#dat_shp, )
                    aq, by = c("grid_id", "week"))

small_poll <- dat_aq_shp |>
  filter(grid_id %in% c(1292159, 1293358, 1293359, 1293360))
aq <- aq |>
  filter(grid_id %in% c(1292159, 1293358, 1293359, 1293360))

write.csv(small_poll, "grid-week-cases.csv", row.names = FALSE)
write.csv(aq, "grid-week-poll.csv", row.names = FALSE)

write.csv(dat_aq_shp, "all-grid-week-cases.csv", row.names = FALSE)
write.csv(aq, "all-grid-week-poll.csv", row.names = FALSE)

sort(unique(small_poll$week))
sort(unique(grid_dat$week))


dat_aq_shp <- st_drop_geometry(dat_aq_shp)

head(aq)

slc_shp <- slc_shp |>
  filter(GEOID == )

# dat_aq_shp <- 


m <- glm(case ~ 1 + offset(denom),
         family = "poisson",
         data = grid_dat)
check_zeroinflation(m) # no
check_overdispersion(m) # yes

skewness(grid_dat$denom)
# log transformation, for skewness
grid_dat$denom_log <- log(grid_dat$denom)
skewness(grid_dat$denom_log)
# mm that didn't help much

m <- glm.nb(case ~ 1 + offset(denom),
         data = dat_aq_shp)
check_overdispersion(m) # it's close, but no

summary(m)
exp(m$coefficients)


## build the cross-basis of predictors and lags
cb1.pm <- crossbasis(dat_aq_shp$max_pm, 
                     lag = 40, 
                     argvar = list(fun="ns", knots = 4), 
                     arglag = list(fun="ns", knots = 4))
summary(cb1.pm)
cb1.no2 <- crossbasis(dat_aq_shp$max_no2, 
                     lag = 40, 
                     argvar = list(fun = "lin"), 
                     arglag = list(fun="ns", knots = 3))
summary(cb1.no2)


case_grid <- aggregate(dat_aq_shp$case, by = list(dat_aq_shp$grid_id), FUN = sum)

dat_aq_shp_small <- dat_aq_shp[which(dat_aq_shp$grid_id == 1829016), ]

# model
m <- glm.nb(case ~ 1 + cb1.pm + offset(denom), # (1 | week)
            data = dat_aq_shp)
# glmer.nb
# crosspred(cb1.pm, m, at=0.7:197, bylag=1, cumul=TRUE)

summary(m)

pred1.pm <- crosspred(cb1.pm, m, at=2.5:141, bylag=1, cumul=TRUE)
pred1.no2 <- crosspred(cb1.no2, m, at=0.5:190, bylag=1, cumul=TRUE)

plot(pred1.pm, col = "blue")
plot(pred1.no2)

summary(m)
summary(pred1.pm$predvar)

pred1.pm <- crosspred(cb1.pm, m, at=70, bylag=1, cumul=TRUE)
pred1.no2 <- crosspred(cb1.no2, m, at=150, bylag=1, cumul=TRUE)

plot(pred1.pm,var=70,xlim=c(0,40),xlab="Lag (years)",ylab="HR for 100 WLM/year",
     ylim=c(0.9,1.3),lab=c(8,5,5),col=1,lwd=1.5,ci="n")
plot(pred1.no2,var=150,xlim=c(0,40),xlab="Lag (years)",ylab="HR for 100 WLM/year",
     ylim=c(0.9,1.3),lab=c(8,5,5),col=1,lwd=1.5,ci="n")


## with inla
system.time(
  n <- inla(case ~ 1 + cb1.pm + f(week, model = "rw1") + 
              f(grid_id, model = "iid") + offset(denom),
            data = dat_aq_shp, family = "nbinomial")
)


