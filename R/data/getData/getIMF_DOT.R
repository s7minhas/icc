if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

# pull from IMF api
devtools::install_github(
	"datadotworld/data.world-r", 
	build_vignettes = TRUE, force = TRUE)

# insert api token
# data.world::set_config(
# 	data.world::save_config(auth_token = "Insert token")
# 	)

# pull data
library(data.world)
# Datasets are referenced by their URL or path
dataset_key <- "https://data.world/imf/direction-of-trade-statistics-dots"
# List tables available for SQL queries
tables_qry <- data.world::qry_sql("SELECT * FROM Tables")
tables_df <- data.world::query(tables_qry, dataset = dataset_key)
# See what is in it
tables_df$tableName

if (length(tables_df$tableName) > 0) {
  sample_qry <- data.world::qry_sql(sprintf("SELECT * FROM `%s`", tables_df$tableName[[1]]))
  sample_df <- data.world::query(sample_qry, dataset = dataset_key)
  sample_df
}