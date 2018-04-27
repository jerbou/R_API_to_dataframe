# TEST de script pointant vers un api

# ==== 00 Installation des libraire ===
# https://www.rstudio.com/resources/videos/using-web-apis-from-r/
# require.library(plyr)
# require.library(httr)
# require.library(jsonlite)
# require.library(magrittr)
# require.library(dplyr)
# require.library(data.tree)
# require.library(tidyr)
# require.library(dplyr)
# require.library(XML)
# require.library(plyr)
# require.library(xmlparsedata)
# require.library(data.table)
# require.library(dtplyr)
# require.library(WriteXLS)
# require.library(xlsx)
# require.library(XML2R)
# require.library(xml2)
# # options(encoding = "UTF-8")
# require.library(readr)
# require.library(dplyr)
# require.library(magrittr)
# require.library(purrr)
# require.library(stringr)
# require.library(stringi)
# require.library(purrr)

# ==== 00 Chargement des librairies ====
require.library(

# ==== 10 Chargement des librairies ====
# https://www.rstudio.com/resources/videos/using-web-apis-from-r/
library(plyr)
library(httr)
library(jsonlite)
library(magrittr)
library(dplyr)
library(data.tree)
library(tidyr)
library(dplyr)
library(XML)
library(plyr)
library(xmlparsedata)
library(data.table)
library(dtplyr)
library(WriteXLS)
library(xlsx)
library(XML2R)
library(xml2)
# options(encoding = "UTF-8")
library(readr)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)
library(stringi)
library(purrr)

# === 20 GET sur l'API pour requeter les infos ====
# doc API utilise
# https://www.data.gouv.fr/fr/reuses/annuaire-des-etablissements-publics-de-ladministration/
mairie21 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/21/mairie')
mairie25 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/25/mairie')
mairie39 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/39/mairie')
mairie58 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/58/mairie')
mairie70 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/70/mairie')
mairie71 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/71/mairie')
mairie89 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/89/mairie')
mairie90 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/90/mairie')

epci70 <-GET('http://etablissements-publics.api.gouv.fr/v1/organismes/70/epci')

# https://stackoverflow.com/questions/36454638/how-can-i-convert-json-to-data-frame-in-r?rq=1
# content(tesi, as='parsed', encoding = 'UTF-8')

# === 30 Creation fonction parse ====
json_parse <- function(req) {
  text <- content(req, as='text', encoding="UTF-8")
    if(identical(text, "")) warn("No output to parse")
  fromJSON(text, simplifyDataFrame = TRUE, flatten = TRUE)
}

# ==== 40 applatissement de json ====
df_mairie21 <- jsonlite::flatten( as.data.frame(json_parse(mairie21)))
df_mairie25 <- jsonlite::flatten( as.data.frame(json_parse(mairie25)))
df_mairie39 <- jsonlite::flatten( as.data.frame(json_parse(mairie39)))
df_mairie58 <- jsonlite::flatten( as.data.frame(json_parse(mairie58)))
df_mairie70 <- jsonlite::flatten( as.data.frame(json_parse(mairie70)))
df_mairie71 <- jsonlite::flatten( as.data.frame(json_parse(mairie71)))
df_mairie89 <- jsonlite::flatten( as.data.frame(json_parse(mairie89)))
df_mairie90 <- jsonlite::flatten( as.data.frame(json_parse(mairie90)))
# tesa_json <- json_parse(tesa)

# df_mairie_bfc <- bind_rows(df_mairie21, df_mairie25, df_mairie39, df_mairie58, df_mairie70, df_mairie71, df_mairie89, df_mairie90)
# Error in bind_rows_(x, .id) : 
# Column `features.properties.CoordonnéesNum.Téléphone` can't be converted from character to list
# soluce
# https://stackoverflow.com/questions/46789010/error-in-bind-rows-x-id-column-cant-be-converted-from-factor-to-numeric/

# ==== 41 compilation pour obtenir un df regional ====
df_mairie_bfc <- rbind.fill(df_mairie21, df_mairie25, df_mairie39, df_mairie58, df_mairie70, df_mairie71, df_mairie89, df_mairie90)
df_mairie_bfc <- as_data_frame(df_mairie_bfc)
# https://github.com/tidyverse/purrr/issues/265

# ==== 50 export de fichier table pour info CD21 ====
# on convertit la liste adresse en character
df_mairie21$features.properties.Adresse.Ligne <- as.character(df_mairie21$features.properties.Adresse.Ligne)

df_mairie_bfc$features.properties.Adresse.Ligne <- as.character(df_mairie_bfc$features.properties.Adresse.Ligne)

# on repere la colonne des plages pour enlever
grep("features.properties.Ouverture.PlageJ", colnames(df_mairie21))
grep("features.properties.Ouverture.PlageJ", colnames(df_mairie_bfc))
myvars <- names(df_mairie21) %in% c("type", "features.properties.Ouverture.PlageJ", "features.geometry.coordinates", "features.type" , "features.geometry.type")
df_cd21 <- as_data_frame(df_mairie21[!myvars])
# df_cd21 <- df_mairie21[,-25]
# df_cd21 <- df_cd21[,-1]

# ==== 51 renommage des champs pour faciliter la lisibilite ====
# df_cd21 <- df_cd21 %>% rename_all(~sub('features.geometry.','',.x))
df_cd21 <- df_cd21 %>% rename_all(~sub('features.properties.','',.x))
names(df_cd21)


# ==== 52 on customise pour avoir les liens url de l annuaire ====
# creation du lien internet pointant vers l annuaire 
# df1[c("lien_annuaire")] <- NA
df_mairie21[c("lien_annuaire")] <- NA
df_cd21  <- transform(df_cd21, "lien_annuaire" = ifelse (substr(df_cd21$codeInsee,1,2)=='21' , paste('https://lannuaire.service-public.fr/bourgogne-franche-comte/cote-d-or/', df_cd21$id , sep="") , lien_annuaire))
#df_cd21  <- transform(df_cd21, "lien_annuaire" = ifelse (substr(df_cd21$codeInsee,1,2)=='70' , paste('https://lannuaire.service-public.fr/bourgogne-franche-comte/haute-saone/', df_cd21$id , sep="") , lien_annuaire))
# df_cd21  <- transform(df_cd21, "lien_annuaire" = ifelse (substr(df_cd21$codeInsee,1,2)=='90' , paste('https://lannuaire.service-public.fr/bourgogne-franche-comte/territoire-de-belfort/', df_cd21$id , sep="") , lien_annuaire))

names(df_cd21)
str(df_cd21)

# ==== 53 Phase finale d ecriture ====
setwd("C:/COPY_data_local/adm_premier_min/annuaire_service_public/")
write.csv(df_cd21, "annuaire_communes_18_04_2018.csv", row.names=FALSE, na = "\t")


# ==== 60 TO DO restructurer les nested list des jour et heures d ouvertures ====
# toujours des nested listes dans le df, veille sur purr ====
# https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html
# https://stackoverflow.com/questions/30270946/arguments-imply-differing-number-of-rows-2-4-3-5

# data.frame(word = do.call(c,df_mairie_bfc$features.properties.Ouverture.PlageJ),
#            group = rep(1:length(df_mairie_bfc$features.properties.Ouverture.PlageJ),
#                        sapply(df_mairie_bfc$features.properties.Ouverture.PlageJ, length)))

tidyr::unnest(df_mairie_bfc$features.properties.Ouverture.PlageJ)

# on unnest les listes pour en obtenir un df presl exploitable
df2 <- unnest(df_mairie_bfc, id=df_mairie_bfc$features.properties.id)
# FAIL A FAIRE un vrai df

# https://stackoverflow.com/questions/27930883/converting-elements-in-a-nested-list-to-dataframe?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

tos <- data.frame(stri_list2matrix(df_mairie_bfc$features.properties.Ouverture.PlageJ, byrow=TRUE), stringsAsFactors=FALSE)
# CA MARCHE ! WIP

# http://bioinfoblog.it/2015/02/the-most-useful-r-command-unnest-from-tidyr/
# df2 %>% mutate()

# ==== 67 reconstruire la structure de la table d ouverture =====
df2[c("lundi","mardi","mercredi","jeudi","vendredi", 'samedi')] <- NA

#??? df2 <- transform(df2,"vendredi"= ifelse(features.properties.Ouverture.PlageJ =="lundi", paste(lundi, heur4_o, heur4_f, sep=" - "), lundi))

gregexpr("vendredi" , df2$features.properties.Ouverture.PlageJ)
str_locate_all("vendredi" , df2$features.properties.Ouverture.PlageJ)

View(head(df2))

# on recherche les vendredi en sortie en matrix
cbind(df2$features.properties.id , stringr::str_extract_all(df2$features.properties.Ouverture.PlageJ, "vendredi", simplify="TRUE"))
stringr::str_extract_all(df2$features.properties.Ouverture.PlageJ, '(début = c("', simplify="TRUE")

stringr::str_extract_all(df2$features.properties.Ouverture.PlageJ, "vendredi", simplify="TRUE")

stringr::str_extract_all(df2$features.properties.Ouverture.PlageJ, simplify="TRUE")

grep("vendredi",df2$features.properties.Ouverture.PlageJ)

getwd()
write.xlsx(df2, 'testou.xlsx')

# df3 <- flatten_df(df2)
# teso <- map_df(map_chr(df_mairie_bfc$features.properties.Ouverture.PlageJ), extract , c("num", "voie", "cp", "ville"))

# https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html


# pour voir les listes
# map(df_mairie_bfc$features.properties.Ouverture.PlageJ, "début")


map_df(df_mairie_bfc$features.properties.Ouverture.PlageJ, extract, c("début", "fin" , "PlageH.début" , "PlageH.fin"), .id=df_mairie_bfc$features.properties.id)

map_chr(df_mairie_bfc$features.properties.Ouverture.PlageJ, "début")







str(df_mairie_bfc)

tail(df_mairie_bfc$features.properties.Adresse.Ligne)





# https://stackoverflow.com/questions/46818672/convert-nested-list-into-data-frame-with-different-column-length

tesi_df <- flattenList(unlist(tesi_json$features))

lists <-  sapply('http://etablissements-publics.api.gouv.fr/v1/organismes/70/epci', jsonlite::fromJSON)

df_list <- as_data_frame(lists)
str(lists$features)

tib_tesi <- as_data_frame(tesa_json$features)






teso <- do.call(rbind, lapply(tesi_json, data.frame, stringsAsFactors=FALSE))

# https://stackoverflow.com/questions/45452015/how-to-convert-list-of-list-into-a-tibble-dataframe

tibble(
  pair = map(tesi_json$features, "properties.id"),
  genes_vec = map_chr(tesi_json$features, "properties.Ouverture.PlageJ")
) %>% 
  mutate(
    pair1 = map_chr(pair, 1),
    pair2 = map_chr(pair, 2) 
  ) %>%
  select(pair1, pair2, genes_vec)



getwd()
deparse(tesa_json$features)
write_csv(tib_tesi, "C:/COPY_data_local/adm_premier_min/annuaire_service_public/all_20180217/organismes/70/test_sortie/test_out_tib.csv")
# https://github.com/tidyverse/readr/issues/303


tesi_json2 <- json_parse(tesi_json$features$properties.Ouverture.PlageJ)
names(tesi_json)
length(tesi_json$features)
length(tesi_json$type)

tesi_json$type
tesi_json$features

typeof(tesi_json)
# https://stackoverflow.com/questions/37996827/convert-json-file-to-a-csv-file-using-r
flatten(tesi_json$properties)


flatten(tesi_json)

tesi_json <- lapply(tesi_json, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
dfout <- do.call("rbind", tesi_json)

tesi_json[2:8]

busi <- as.data.frame(t(sapply(tesi_json, fromJSON)))


busi <- as_data_frame(t(sapply(tesi_json, fromJSON)))

# https://stackoverflow.com/questions/16947643/getting-imported-json-data-into-a-data-frame

df3 <-as.data.frame(tesi_json$features)

tail(tesi_json)

dfout2 <- dplyr::bind_rows(t(tesi_json))

df_tesi <- as_data_frame(tesi_json$features$properties)


str(content(tesi))

df_tesi <- as_data_frame(tesi_json$type)

headers(tesi)

write.csv(df_tesi, 'tesi_json.csv')

write.csv(df3, 'df3.csv')


getwd()
# ==== 999 brouillon ====
# test pour applatir le json
# df_tesi <- flatten( as.data.frame(tesi_json))
#          recursive = TRUE)
# df_palage <- flatten( as.data.frame(df_mairie_bfc$features.properties.Ouverture.PlageJ))
