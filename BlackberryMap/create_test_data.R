# Authors: Chris Madsen, Saeesh Mangwani
# Date: 2022-07-30

# Description: Creating test data for shiny app

# ==== Code ====
dat = data.frame(location = "Sidney Airport - Stirling Way",
                 productivity = "High")

dat$patch_number = nrow(dat)

dat = st_set_geometry(dat, st_geometry(st_cast(c(st_point(c(48.64726984376748, -123.40917029173556)),
                                                 st_point(c(48.64734072836236, -123.40892352850621)),
                                                 st_point(c(48.6468516226296, -123.40661682875333)),
                                                 st_point(c(48.64527794591409, -123.40584435255703)),
                                                 st_point(c(48.64468957669249, -123.40657391340909)),
                                                 st_point(c(48.64468248785449, -123.40792574675264)),
                                                 st_point(c(48.64599325820387, -123.40891219680323))), "POLYGON")))

dat = st_set_crs(dat, value = 4326)

my_geom = st_as_text(st_sfc(dat$geometry)) #Get geometry as text.
my_fields = st_drop_geometry(dat)
dat_to_write = cbind(my_fields, my_geom)
dat_to_write = dat_to_write %>%
  mutate(patch_number = row_number())

repo <- repository("F:/R Projects/blackberrymapping.github.io")
write_vc(dat_to_write, file = "blackberry_patches",
         #strict = F,
         root = repo, stage = TRUE)
commit(repo, all = T, message = paste0("Updated data on ",Sys.time()))
#push(repo)

write_sf(dat, "blackberry_patches.gpkg")


