library(data.table)
library(collapse)

# load data ----
## Auxiliary data ---------

pipload::pip_load_all_aux(replace = TRUE)


## microdata --------
years <- c(2019)
ind <- lapply(years,
              \(x) pipload::pip_load_data("IND", x))


cache <- lapply(years,
             \(x) pipload::pip_load_cache("IND", x))

df <- cache[[1]]

# basic calculations --------

## population factor




df[,
   .SD[which.min(pop_fact)],
   by = reporting_level
   ][,
     .(reporting_level, pop_fact)
     ]











