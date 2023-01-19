# Will probably be removed - testing thing



# df <- data.frame(var1 = c("a", "b", "c"),
#                  var2 = c(1,2,3), 
#                  var3 = c("d", "e", "f"))
# 
# tmp <- data.frame(Variable = c("var1", "var2", "var3"), 
#                   Format = c("character", "numeric", "character"),
#                   New_format = c("character", "integer", "factor"))
# 

# types <- paste(map_chr(tmp$New_format, ~str_sub(., 1,1)), collapse = "")

types2 <- paste(map_chr(var_type_header, ~str_sub(.,1,1)), collapse="")

new_df <- type_convert(AU_premerged_covid, types2, guess_integer = T)

names(fileList)

str(new_df)