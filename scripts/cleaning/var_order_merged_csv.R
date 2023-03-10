# Read in template
# This to import merging template into env for variable structure

template <- read.csv(file='data/template.csv')

# This is to set the order of vars in csvs

order_header_premerge <- c(
     "id", 
     "data_source",
     "country",
     "hemisphere",
     "week",
     "year",
     "age_group",
     "hsp_rate",
     "hsp_rate_flu",
     "hsp_rate_rsv",
     "hsp_rate_covid19",
     "cases_rate_flu",
     "cases_rate_rsv",
     "cases_rate_covid19",
     "hsp_abs",
     "hsp_abs_flu",
     "hsp_abs_rsv",
     "hsp_abs_covid",
     "cases_abs_flu",
     "cases_abs_rsv",
     "cases_abs_covid" ,
     "denominator",
     "subtype_a_abs",
     "subtype_b_abs",
     "subtype_c_abs",
     "subtype_a_rate",
     "subtype_b_rate",
     "subtype_c_rate"
)

order_header_premerge_epiweeks <- c(order_header_premerge, "epi_dates")

var_type_header <- c(
        id = "integer",
        data_source = "character",
        country = "character",
        hemisphere = "character",
        week = "integer",
        year = "integer",
        age_group = "character",
        hsp_rate = "double",
        hsp_rate_flu = "double",
        hsp_rate_rsv = "double",
        hsp_rate_covid19 = "double",
        cases_rate_flu = "double",
        cases_rate_rsv = "double",
        cases_rate_covid19 = "double",
        hsp_abs = "double",
        hsp_abs_flu = "double",
        hsp_abs_rsv = "double",
        hsp_abs_covid = "double",
        cases_abs_flu = "double",
        cases_abs_rsv = "double",
        cases_abs_covid = "double",
        denominator = "double",
        subtype_a_abs = "double",
        subtype_b_abs = "double",
        subtype_c_abs = "double",
        subtype_a_rate = "double",
        subtype_b_rate = "double",
        subtype_c_rate ="double"
)