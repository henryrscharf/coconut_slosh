library(sf)
library(readxl)
library(tigris)
library(tidycensus)
library(RColorBrewer)
options(tigris_use_cache = TRUE)
income <- get_acs("tract", year = 2021, state = "AZ", county = "Pima", 
                  variables = c(income = "B06011_001"), geometry = T, cache_table = T)
# plot(income['estimate'], xlim = c(-111, -110.4477))
boundaries_utm <- st_read("School_Attendance_Areas/School_Attendance_Areas.shp")
boundaries <- st_transform(boundaries_utm, st_crs(income))
names(boundaries) <- sapply(names(boundaries), function(name) gsub(" ", "_", name))
names(boundaries) <- sapply(names(boundaries), function(name) gsub("-", "", name))
schools_utm <- st_read("Schools/Schools.shp")
schools_all <- st_transform(schools_utm, st_crs(income))
schools_K8 <- schools_all[schools_all$GRADELVL %in% c("2-8", "3-12", "3-5", "3-8", 
                                                      "4-6", "4-8", "5-12", "5-8", 
                                                      "6-8", "7-8", "K-12", "K-5", 
                                                      "K-6", "K-8", "PK-5", "PK-6", "PK-8"), ]
schools <- schools_K8[schools_K8$SCHNAME %in% boundaries$NAME, ]
boundaries <- boundaries[boundaries$NAME %in% schools_K8$SCHNAME, ]
schools <- st_intersection(schools, income)
names(schools) <- sapply(names(schools), function(name) gsub(" ", "_", name))
names(schools) <- sapply(names(schools), function(name) gsub("-", "", name))
boundaries <- merge(boundaries, as.data.frame(schools)[, c("SCHNAME", "estimate")], 
                    by.x = "NAME", by.y = "SCHNAME")
grades <- as.data.frame(read_xlsx("FY 22 Combined A-F Public File 2023-01-24.xlsx", sheet = "Traditional K-8 Schools"))
names(grades) <- sapply(names(grades), function(name) gsub(" ", "_", name))
names(grades) <- sapply(names(grades), function(name) gsub("-", "", name))
grades_pima <- grades[grades$County == "Pima", ]
matches <- sapply(boundaries$NAME, function(name){
  sub_names <- unlist(sapply(unlist(strsplit(name, split = " ")), strsplit, split = "-"))
  sub_names_exclude <- sub_names[!(sub_names %in% c("HIGH", "SCHOOL", "MIDDLE", "ELEMENTARY", 
                                                    "JUNIOR", "POLYTECHNIC", "K", 8))]
  match <- unlist(sapply(sub_names_exclude, function(sub_name){
    grep(pattern = sub_name, x = grades_pima$School_Name, ignore.case = T)
  }))
  match_table <- table(match)
  if(length(match_table) > 0){
    if(max(match_table) > 1) return(as.numeric(names(match_table)[which.max(match_table)]))
  }
  if(length(match) == 0){
    return(NA)
  }
  if(length(match) == 2){
    out <- intersect(match[[1]], match[[2]])
    if(length(out) < 1) out <- match
  } else if(length(match) == 3){
    out <- intersect(match[[1]], match[[2]])
    out <- intersect(out, match[[3]])
    if(length(out) < 1) out <- match
    } else {
    out <- match
  }
  return(out)
})
n_matches <- sapply(matches, length)
table(n_matches)
head(matches[which(n_matches == 4)])
boundaries$School_Code <- NA
boundaries$School_Code[which(n_matches == 1)] <- grades_pima$School_Code[unlist(matches[which(n_matches == 1)])]
boundaries <- merge(boundaries, grades_pima, by = "School_Code")
# plot(boundaries["Letter Grade"], xlim = c(-111, -110.4477), 
#      pal = adjustcolor(RColorBrewer::brewer.pal(5, "RdBu"), alpha = 0.5))

library(mgcv)
plot(K8_Total_Percentage_Earned ~ estimate, data = boundaries)
fit <- gam(K8_Total_Percentage_Earned ~ s(estimate), data = boundaries)
est_grid <- seq(5000, 70000, l = 5e1)
pred <- predict(fit, newdata = data.frame(estimate = est_grid), se.fit = T)
lines(est_grid, pred$fit, lwd = 2)
lines(est_grid, pred$fit + pred$se.fit * qnorm(0.025), lty = 2)
lines(est_grid, pred$fit + pred$se.fit * qnorm(0.975), lty = 2)
points(K8_Total_Percentage_Earned ~ estimate, 
       data = boundaries[boundaries$NAME %in% c("SAM HUGHES ELEMENTARY SCHOOL",
                                                "CARRILLO K-5 COMMUNICATION AND CREATIVE ARTS MAGNET SCHOOL",
                                                "BORTON MAGNET SCHOOL",
                                                "HOLLINGER K-8 SCHOOL"), ], 
       pch = 16, col = hcl.colors(4), cex = 2)
boundaries$resid <- fit$residuals
schools <- merge(schools, as.data.frame(boundaries)[, c("NAME", "School_Code", "resid")], 
                 by.x = "SCHNAME", by.y = "NAME")
schools <- merge(schools, grades_pima, by = "School_Code")
breaks <- seq(-30, 30, l = 9)
pal <- brewer.pal(8, name = "RdBu")
# pal[1:4] <- NA
xlim_ll <- c(-111.01, -110.75)
ylim_ll <- c(32.145, 32.32)
map <- ggmap::get_stamenmap(c(xlim_ll[1], ylim_ll[1], xlim_ll[2], ylim_ll[2]), zoom = 13)
xlim <- c(-12350000, -12338000)
ylim <- c(3783000, 3804000)
boundaries_map <- st_transform(boundaries, st_crs(3857))
schools_map <- st_transform(schools, st_crs(3857))
plot(boundaries_map['resid'], breaks = breaks, pal = adjustcolor(pal, 0.35), 
     reset = F, bgMap = map, xlim = xlim, ylim = ylim)
plot(schools_map['resid'], add = T, pch = 16, cex = 0.5, breaks = breaks, pal = pal)
head(boundaries$NAME[order(boundaries$resid, decreasing = T)], 50)

plot(boundaries_map['resid'], breaks = breaks, pal = adjustcolor(pal, 0.35), 
     reset = F, bgMap = map, xlim = xlim, ylim = ylim)
plot(schools_map['resid'], add = T, pch = 16, cex = 0.5, breaks = breaks, pal = pal)

## TAKE AWAY
# Some underrated schools: 
#   - Carillo
  - 
