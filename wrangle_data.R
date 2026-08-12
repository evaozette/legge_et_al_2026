
## Notes ----
# Data prep for Eva Legge by Randy Swaty
# May 25, 2026
# Take initial output from ArcGIS combine of LANDFIRE Map Zones, Biophysical Settings and Succession Classes
# Join in helper data such as species, reference percent; calculate current percents


## Dependencies -----

# packages
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)


# read raw data
bps_scls_raw <- read_csv("inputs/bps_scl_apps_mzs.csv")

# read helper data
sclass_species <- read_csv("inputs/sclass_species_conus.csv") |>
  unite(model_label, c(bps_model_id, class)) # for joining

sclass_descriptions <- read_csv("inputs/scls_descriptions.csv") # not used here, but made available in case it's useful
ref_percents <- read_csv("inputs/ref_con_long.csv")


## Initial cleaning of input data ----

bps_scls_clean <- bps_scls_raw |>
  select(-c(apps_mzs_rast, LC20_BPS_220, LC24_SCla_250)) |>  # don't need these
  unite(model_label, c(BPS_MODEL, LABEL), sep = "_", remove = FALSE) |>  # create label
  relocate(model_label, .after = last_col()) |>  # move it to end
  filter(LABEL != "Water") |>  # no myco in water
  clean_names()


## Join in reference percents, calculate current percents ----

# join in reference percents
bps_scls_clean <- bps_scls_clean |>
  left_join(ref_percents |> 
              select(model_label, ref_percent), 
            by = "model_label")

# calculate current percents
bps_scls_clean <- bps_scls_clean |>
  group_by(bps_model) |> # important to use this for grouping due to variants within bps_names that are possible
  mutate(
    total_bps_count = sum(count, na.rm = TRUE),
    cur_percent = round((count / total_bps_count) * 100, 0)) |>
  ungroup() |>
  relocate(total_bps_count, .after = count)

# calculate and indicate percent over or under representation of current compared to reference percent

bps_scls_clean <- bps_scls_clean |>
  mutate(
    class_percent_difference = cur_percent - ref_percent,
    class_representation = case_when(
      cur_percent < ref_percent ~ "under",
      cur_percent > ref_percent ~ "over",
      TRUE ~ "equal"
    )
  )


## Join in S-Class Species and clean ----

bps_scls_clean <- bps_scls_clean |>
  left_join(
    sclass_species,
    by = "model_label",
    relationship = "many-to-many" )

# replace NAs with sclass value per row

bps_scls_clean <- bps_scls_clean |>
  mutate(across(
    c(symbol, scientific_name, common_name, canopy_position),
    ~ coalesce(., label)))


# Write output ----

write.csv(bps_scls_clean, file = "outputs/bps_scls_clean.csv", row.names = FALSE )

##wrangling the FINAL_Myco_Status dataset to create mycorrhizal assignments for each species type


#convert dataframe to long format
myco_status_long <- FINAL_Myco_Status_Eva %>%
  pivot_longer(
    cols = starts_with("CoverType"),
    names_to = "CoverTypeNumber",
    values_to = "CoverType",
    values_drop_na = TRUE
  )


myco_status_long_col <- myco_status_long %>%
  separate(
    CoverType,
    into = c("Species", "Myco_Status"),
    sep = "-"
  )

#check whether there are multiple mycorrhizal statuses per species  
myco_status_long_col %>%
  distinct(Species, Myco_Status) %>%
  count(Species) %>%
  filter(n > 1)%>%
  print(n = 25)

#reconcile myco statuses that are relevant to our region (ARIST -- a bunchgrass, assigned AM, BOCU -- sideoats, asigned AM/nm, SALIX, assigned AM/ECM)
myco_status_long_col <- myco_status_long_col %>%
  mutate(
    Myco_Status = case_when(
      Species == "ARIST" ~ "AM",
      Species == "BOCU"  ~ "AM/NM",
      Species == "SALIX" ~ "AM/ECM",
      TRUE ~ Myco_Status
    )
  )

#delete the other rows with conflicting mycorrhizal statuses since they're not relevant to our study region
myco_status_clean <- myco_status_long_col %>%
  filter(!Species %in% c("ARGL4","ARPA6","ARPU","ATCO4","ATGA","CHSE11","DAPA2","ELEL5","ELELE","JUDE",
                         "JUOC","JUSC2","LUPE","POSA12","POTR","PSME",
                         "QUBE5","QUCO7","QUJO3","RIMO","SYLO","SYOR2"))

#now check again whether there are multiple mycorrhizal statuses per species... nope!
myco_status_clean %>%
  distinct(Species, Myco_Status) %>%
  count(Species) %>%
  filter(n > 1)%>%
  print(n = 25)

#make a new species key with one species and mycorrhizal status per row
myco_key <- myco_status_clean %>%
  distinct(Species, Myco_Status)

#make the bps_scls table compatible with the mycorrhizal status table
bps_scls_clean <- bps_scls_clean %>%
  rename(Species = symbol)

#join the myco key dataframe and the BpS_Scls dataframe
bps_scls_myco <- bps_scls_clean %>%
  left_join(myco_key, by = "Species")

write.csv(bps_scls_myco, file = "outputs/bps_scls_myco.csv", row.names = FALSE )

#fill in missing mycorrhizal statuses and citations

#AM:
###OSRE (Osmunda regalis) = AM, citation = https://www.nature.com/articles/ncomms1831
###COFL2 (Flowering Dogwood) = AM, citation = https://cdnsciencepub.com/doi/10.1139/b86-127
###RHCO (Rhus copallinum, Flameleaf/winged sumac) = AM, citation = https://link.springer.com/article/10.1007/s00572-005-0367-0
###ILVE (Ilex verticillata, common winterverry) = AM, citation = https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1526-100X.1998.00628.x
###RHGL(Rhus glabra, Smooth sumac) = AM, DOI = 0030-0950/79/0006-0274
###CORU (Cornus rugosa, Roundleaf dogwood), assuming AM based on genus
###LIST (Linum striatum, Ridged yellow flax), assuming AM based on genus: https://pmc.ncbi.nlm.nih.gov/articles/PMC3474904/
###VIRA (Viburnum rafinesquianum, Downy arrowwood), assuming AM based on genus: https://link.springer.com/article/10.1007/s00572-006-0064-7
###VAAM3 (Vallisneria americana, American eelgrass) = AM, citation = https://link.springer.com/article/10.1023/A:1017071701679
###TRST4 (Trifolium stoloniferum, Running buffalo clover) = AM based on genus, citation = https://nph.onlinelibrary.wiley.com/doi/10.1111/j.1469-8137.1981.tb01729.x
###SPIRA (Spiraea) = AM, citation = https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1526-100X.1998.00628.x
###SMRO (Smilax rotundifolia, Roundleaf greenbrier) = AM, citation = chrome-extension://oemmndcbldboiebfnladdacbdfmadadm/http://www.ijat-aatsea.com/pdf/v7_n6_11_November/19_IJAT_2011_7_6__Songachan,_Highland%20Kayang_FX_confirmed.pdf
###RUAL (Rubus allegheniensis) = AM, citation = __Dhillion, S. S., & Friese, C. F. (1994). The occurrence of mycorrhizas in prairies: application to ecological restoration. In The Proceedings of the 13th North American Prairie Conference, The University of Windsor (Canada) (pp. 103-114).
##RUCA16 (Rubus canadensis) = AM, citation = general knowledge of rubus species (see above)
##ROSE2 (Climbing rose) = AM,(can't find this specific species, but the genus is AM it seems) Berch, S. M., Gamiet, S., & Deom, E. (1988). Mycorrhizal status of some plants of southwestern British Columbia.
##PRPE2 (Prunus pensylvanica) = AM, citation = https://www.gbif.org/occurrence/2465009798
##POTAM, PORI2, POOB2 (Potamogeton spp) = AM***AQUATIC**, citation = https://link.springer.com/article/10.1672/0277-5212(2003)023[0961:MCAHGI]2.0.CO;2
##POCO14 (Pickerelweed) = AM **AQUATIC**, citation = https://onlinelibrary.wiley.com/doi/10.1111/j.1526-100X.1998.00628.x
##PLMA3 (Plantago maritima) = AM **AQUATIC?*, citation = https://nph.onlinelibrary.wiley.com/doi/10.1111/j.1469-8137.1928.tb07498.x
##ILMU (Ilex mucronata) = AM, citation = https://www.gbif.org/occurrence/2464998014
##GLMA (Sea milkwort) = AM, citation = https://www.gbif.org/occurrence/2465013519
##COVI3 (Virginia dayflower) AM based on genus, citation = https://www.gbif.org/occurrence/2465012678
##CORA6 (Gray dogwood) = AM, citation = https://www.gbif.org/occurrence/2464997942
##CALAM (Reedgrass) = AM, citation = https://www.gbif.org/occurrence/2464999816
##ATPA4 (Spear saltbush) = AM, citation = https://www.gbif.org/occurrence/2465009799
###ASCA2 (British columbia wildginger) = AM based on genus, citation = https://www.gbif.org/occurrence/2464996891
###ACER (Maple) = AM based on genus, main FungalRoot database
###ACBA3 (Acer Barbatum, southern sugar maple) = AM based on genus, main FungalRoot database
###ACSA2 (Acer saccharinum; Silver Maple) = AM based on genus, main FungalRoot database
####ACPE (Acer pensylvanicum; Striped maple) = AM based on genus, main FungalRoot database
###ACNE2 (Acer negundo; Boxelder) = AM based on genus, main FungalRoot database
###ACLE (Acer leucoderme; Chalk maple) = AM based on genus, main FungalRoot database
###FRAM (Fraxinus americana) and FRNI (Fraxinus nigra), FRQU (Fraxinus quadrangulata) = AM based on genus, main FungalRoot database
###CELA (Celtis laevigata; Sugarberry) and CEOC (Celtis occidentalis; Common hackberry) = = AM based on genus, main FungalRoot database
###JUCO6 (Juniperus communis; common juniper) = AM based on genus, main FungalRoot database
###CRATA (Crataegus, Hawthorn) = = AM based on genus, main FungalRoot database
###PRSE (Prunus serotina, Black cherry) = AM based on genus, main FungalRoot database
###RUBUS
###SOAM3
###ULAM
###ANDRO2
###ARBE7
###BAHA
###CACA4
###DACO
###DANTH
###DASP2
###ELVI3
###JUBA = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###JUGE = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###LISP = AM based on genus, main FungalRoot database 
###PTAQ
###SCHIZ4
###VIBUR
###BOCU
###CALA16 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CALA11 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CAOL3 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CAST8 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CORNU
###COAM2
###COSE16
###ELRO2 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###MYGA = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###SCAC3 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
##SYOR
###TYLA
###WOVI
###SACE *wetland plant
###CLMAJ = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###FOLI 
###HEDU2 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CABI5 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CAREX = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CAST41 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CATR10= AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###CAUT= AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###ARST5
###CLMO2
###CYRA
###ELCO2 = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###GALA = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###SPAL = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM)
###ROPS = AM based on genus, main FungalRoot database (origionally assigned EcM in kitty's spreadsheet...)
###SAVI = AM based on genus, main FungalRoot database (technically NM-AM in the database but probs ok to assign AM... it was NM in kitty's spreadsheet)

#ECM
###QUPR (Quercus prinoides) = ECM citation = https://www.gbif.org/occurrence/2465015017
###PINUS = ECM (based on general knowledge)
###LARIX = ECM, citation = https://www.gbif.org/occurrence/2464997687
###COCO6 = ECM, citation = https://www.gbif.org/occurrence/2465001031
###BEPO (Gray birch) = ECM, citation = https://www.gbif.org/occurrence/2464990157
###ALNUS, ALIN2, ALINR (Alder, Grey alder, Speckled alder) = EM based on FungalRoot and https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.12170
###BETUL (Betula) = EcM based on main fungalroot database, changing from origional ECM/am
###BELE (Betula lenta) = EcM based on genus of main fungalroot database and https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1061-2971.2004.00255.x, changing from origional assignment (AM/ecm/nm)
###BENI (Betula nigra), BEPU4 (Betula pumila) = EcM based on genus in main fungalroot database
###FAGR (Fagus grandifolia) = EcM based on EcM in main fungalroot database
##CORYL (Corylus; Hazelnut), COAM3 (Corylus americana; American hazelnut) = EcM based on genus in main FungalRoot
##TSUGA, TSCA (Tsuga; Tsuga canidensis) = EcM based on genus in main FungalRoot
##TIAM (Tilia americana; American basswood) = EcM based on genus in main FungalRoot
##CAGL8 (Carya glabra; Pignut hickory) = EcM based on fungalroot: http://gbif.org/occurrence/2465008437
##CAAL27(Carya alba; Mockernut hickory) = EcM based on fungalroot: https://www.gbif.org/occurrence/2465006206


#ERM:
###ZENOB (Zenobia, Honeycup), assuming ERM based on family
###VACO (Vaccinium corymbosum, Highbush Blueberry) = ERM, citation = https://journals.ashs.org/view/journals/hortsci/38/6/article-p1163.xml
###RHMI2 and RHMA4 (Rhododendron minus, Rhododendron maximum) = ERM based on other genus assignments
###PHCA10 (Phyllodoce caerulea) = ERM, citation = https://www.gbif.org/occurrence/2465012139
###DILA (Pincushion plant) = ERM, citation = https://www.gbif.org/occurrence/2464991228#event
###Vaccinium spp = ErM, citation = https://www.gbif.org/occurrence/2464998964
###CHAMA5 (Leatherleaf) = EcM, citation = https://www.gbif.org/occurrence/2465000852


##NM:
###SPHAG2 (Sphagnum) = NM, citation = https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.13993
###SAVI (Salicornia virginica, Virginia glasswort), citation = https://link.springer.com/chapter/10.1007/978-3-662-08897-5_27
###NULUV (Varigated yellow pond-lily), = NM **AQUATIC?** citation = https://www.gbif.org/occurrence/2465005151
###GALA (Red hempnettle) = NM based on genus, citation = https://www.gbif.org/occurrence/2465014471
###ELCO2 (Flatstem spikerush) = NM based on genus, citation = https://www.gbif.org/occurrence/2465020982
###CAUT (Carex utriculata) = NM, citation = https://www.gbif.org/occurrence/2464990666
###CATR10 (Carex trisperma) = NM, citation = https://www.gbif.org/occurrence/2465002768
###CAST41 (Walter's sedge) = NM based on genus (but could also be AM??)
##CABI5 (Bigelow’s sedge) = NM, citation = https://www.gbif.org/occurrence/2465011726


##Dual Mycorrhizal
#### SALIX, SAHU2 (Salix humilis, prarie willow), based on other salix assignments
####All populus (POPUL, Populus; PODE3, populus deltoides; POTR5, populus tremuloides; POGR4, populus grandidentata) = DM based on genus, main fungalroot database

##UNKNOWN
###HEDU2 (Grassleaf mudplantain), unknown AQUATIC 
###FOLI (Upland swampprivet) = UNKNOWN

#assign developed, ag, UN, and UE to those mycorrhizal statuses

bps_scls_myco_assign <- bps_scls_myco %>%
  mutate(
    Myco_Status = case_when(
      Species %in% c("OSRE", "COFL2", "RHCO", "ILVE", "RHGL", "CORU", "LIST", "VIRA", "VAAM3", "TRST4", "SPIRA", "SMRO", 
                     "RUAL", "RUCA16", "ROSE2", "PRPE2", "POTAM", "PORI2", "POOB2", "POCO14", "PLMA3", "ILMU", "GLMA", "COVI3", 
                     "CORA6", "CALAM", "ATPA4", "ASCA2", "ACSP2", "ACER", "ACBA3", "ACLE", "ACNE2", "ACPE", "ACSA2", "FRAM2", 
                     "FRNI", "FRQU", "CELA", "JUCO6", "CEOC", "CRATA", "PRSE2", "RUBUS", "SOAM3", "ULAM", "ANDRO2", "ARBE7", 
                     "BAHA", "CACA4", "DANTH", "DACO", "DASP2", "ELVI3", "JUBA", "JUGE", "LISP", "PTAQ", "SCHIZ4", "VIBUR", 
                     "BOCU", "CALA16", "CALA11", "CAOL3", "CAST8", "CORNU", "COAM2", "COSE16", "ELRO2", "MYGA", "SCAC3", 
                     "SYOR", "TYLA", "WOVI", "SACE", "CLMAJ", "FOLI", "HEDU2", "CABI5", "CAREX", "CAST41", "CATR10", "CAUT", 
                     "ARST5", "CLMO2", "CYRA", "GALA", "SPAL", "ELCO", "ROPS", "SAVI") ~ "AM",
      Species %in% c("ZENOB", "VACO", "RHMA4", "RHMI2", "PHCA10", "DILA", "CHAMA5") ~ "ERM",
      Species %in% c("SPHAG2", "SAVI", "NULUV", "GALA", "ELCO2", "CAUT", "CATR10", "CAST41", "CABI5") ~ "NM",
      Species %in% c("SALIX", "SAHU2", "POPUL", "PODE3", "POTR5", "POGR4") ~ "DM",
      Species %in% c("QUPR", "PINUS", "LARIX", "COCO6", "BEPO", "ALNUS", "ALIN2", "ALINR", "BETUL", "BELE", "BENI", "BEPU4", "FAGR",
                     "CORYL", "COAM3", "TSUGA", "TSCA", "TIAM", "CAAL27", "CAGL8") ~ "ECM",
      Species %in% c("HEDU2", "FOLI") ~ "UNKNOWN",
      Species == "UN" ~ "UN",
      Species == "UE" ~ "UE",
      Species == "Developed" ~ "Developed",
      Species == "Barren or Sparse" ~ "Barren_Sparse",
      Species == "Agriculture" ~ "Agriculture",
      scientific_name == "Quercus spp" ~ "ECM",
      scientific_name == "Vaccinium spp." ~ "ERM",
      TRUE ~ Myco_Status
    )
  )

#write output for myco_assign--- 
write.csv(bps_scls_myco_assign, file = "outputs/bps_scls_myco_assign_final.csv", row.names = FALSE )

#assign woody 
bps_scls_myco_assign_woody <- bps_scls_myco_assign %>%
  mutate(Type = case_when(
    Species %in% c("ABBA", "ABFR", "ACER", "ACBA3", "ACLE", "ACNE2", "ACPE", "ACRU", "ACSA2", "ACSA3", "ACSP2", "ALNUS", "ALIN2", 
                   "ALINR", "BAHA", "BETUL", "BEAL2", "BELE", "BENI", "BEPA", "BEPAC2", "BEPO", "BEPU4", "CARYA", "CAAL27", "CACA38", 
                   "CAGL8", "CAOV2", "CADE12", "CELA", "CEOC", "CEOC2", "CECA4", "CHLA", "CHTH2", "CHAMA5", "CLMO2", "CORNU", "COAM2",
                   "COFL2", "CORA6", "CORU", "COSE16", "CORYL", "COAM3", "COCO6", "CRATA", "CYRA", "FAGR", "FOLI", "FRAM2",
                   "FRNI", "FRPE", "FRQU", "GAYLU", "GABA", "GAYLU", "GOLA", "HUTO", "ILGL", "ILMU", "ILVE", "ILVO", "IVFR", "IVIM", "JUNI",
                   "JUAS", "JUCO6", "JUVI", "JUVIV", "KALA", "LARIX", "LALA", "LEGR", "LEPH11", "LIQUI", "LIST2", "LIRIO", "LITU", 
                   "LYLU3", "MAGR4", "MAVI2", "MOCE2", "MOPE6", "MYGA", "NYBI", "NYSY", "OSVIV", "PEPA37", "PHCA10", "PIGL", "PIMA", "PIRU",
                   "PINUS", "PIBA2", "PIEC2", "PIEL", "PIPA2", "PIPU5", "PIRE", "PIRI", "PISE", "PIST", "PITA", "PIVI2", "PLATA", 
                   "PLOC", "POPUL", "POGR4", "PODE3", "POTR5", "PRPE2", "PRSE2", "QUERC", "QUAL", "QUBI", "QUCO2", "QUEL", "QUFA", "QUHE2",
                   "QUIL", "QUIN", "QULA", "QULA2", "QULA3", "QULY", "QUMA2", "QUMA3", "QUMU", "QUNI", "QUPA5", "QUPA", "QUPA2",
                   "QUPH", "QUPR", "QUPR2", "QURU", "QUSH", "QUST", "QUTE", "QUVE", "QUVI", "RHCA8", "RHMA4", "RHMI2", "RHUS", "RHAR4", "RHCO", 
                   "RHGL", "ROPS", "ROSE2", "RUBUS", "RUAL", "RUCA16", "SAMI8", "SAPA", "SALIX", "SAHU2", "SAAL5", "SOAM3", "SPAL2",
                   "SYOR", "TAAS", "TADI2", "THOC2", "TIAM", "TSUGA", "TSCA", "ULAM", "VACCI", "VAAR", "VACO", "VAPA4",
                   "VACCI", "VAUL", "VIBUR", "VIRA", "ZENOB") ~ "Woody",
    scientific_name %in% c("Quercus spp", "Vaccinium spp.") ~ "Woody",
    Species %in% c("ACNE", "AMBR", "ANDRO2", "ANGE", "ANVI2", "ARIST", "ARBE7", "ARST5", "ARUND2", "ARGI", "ARGIT8", "ASCA2", 
                   "ATPA4", "BOCU", "CALAM", "CACA4", "CAREX", "CABI5", "CALA16", "CALA11", "CAOL3", "CAPE6", "CAREX", "CAST41", 
                   "CAST8", "CATR10", "CAUT", "CHASM", "CLMAJ", "COVI3", "DAGA", "DANTH", "DACO", "DASP2", "DILA", "DISP",
                   "ELCO2", "ELVI3", "EUMA6", "GALA", "GLMA", "HEHI2", "HESP11", "JUBA", "JUGE", "JURO", "LISP", "LIST", "MOCE",
                   "OSRE", "PAAM2", "PAVI2", "PLMA3", "PTAQ", "PYVI", "ELRO2", "SCHIZ4", "SCSC", "SCSCS3", "SCAC3", 
                   "SITE", "SMILA2", "SMRO", "SONU2", "SPAL", "SPPA", "SPPE", "SPHAG2", "SPIRA", "SPVA", "TRST4", 
                   "TRDA3", "UNPA", "WOVI") ~ "Herbaceous",
    Species %in% c("HEDU2", "NULUV", "NYOD", "POCO14", "POTAM", "POOB2", "PORI2", "SALA2", "SAVI", "SACE", "TYLA", "VAAM3") ~ "Aquatic Herbaceous",
    Species %in% c("Agriculture") ~ "Agriculture",
    Species %in% c("Barren or Sparse") ~ "Barren_Sparse",
    Species %in% c("Developed") ~ "Developed",
    Species %in% c("UE") ~ "UE",
    Species %in% c("UN") ~ "UN",
    TRUE ~ NA_character_
  ))

colnames(bps_scls_myco_assign_woody)

#write output for woody_assign--- 
write.csv(bps_scls_myco_assign_woody, file = "outputs/bps_scls_myco_assign_woody.csv", row.names = FALSE )

#make a summarized file for further analysis
bps_scls_myco_assign_woody %>%
  group_by(model_label) %>%
  summarise(
    across(
      everything(),
      ~ n_distinct(.x, na.rm = TRUE)
    )
  )

#make a summary dataframe
bps_scls_summary <- bps_scls_myco_assign_woody %>%
  filter(
    !label %in% c(
      "Agriculture",
      "Barren or Sparse",
      "Developed",
      "UE",
      "UN", 
      "Fill-Not Mapped"
    )
  ) %>%
  mutate(
    across(
      c(fri_replac, fri_mixed, fri_surfac, fri_allfir),
      ~ if_else(.x == -9999, 5000, .x)
    )
  ) %>%
  group_by(model_label) %>%
  summarise(
    bps_model = first(bps_model),
    bps_name = first(bps_name),
    groupveg = first(groupveg),
    fri_replac = first(fri_replac),
    fri_mixed = first(fri_mixed),
    fri_surfac = first(fri_surfac),
    fri_allfir = first(fri_allfir),
    label = first(label),
    ref_percent = first(ref_percent),
    cur_percent = first(cur_percent),
    class_percent_difference = first(class_percent_difference),
    class_representation = first(class_representation),
    AM_woody = sum(Myco_Status == "AM" & Type == "Woody", na.rm = TRUE),
    Dual_woody = sum(Myco_Status == "DM" & Type == "Woody", na.rm = TRUE),
    EcM_woody = sum(Myco_Status == "ECM" & Type == "Woody", na.rm = TRUE),
    ErM_woody = sum(Myco_Status == "ERM" & Type == "Woody", na.rm = TRUE),
    NM_woody = sum(Myco_Status == "NM" & Type == "Woody", na.rm = TRUE),
    AM_herbaceous = sum(Myco_Status == "AM" & Type == "Herbaceous", na.rm = TRUE),
    EcM_herbaceous = sum(Myco_Status == "ECM" & Type == "Herbaceous", na.rm = TRUE),
    NM_herbaceous = sum(Myco_Status == "NM" & Type == "Herbaceous", na.rm = TRUE),
    .groups = "drop"
  )%>%
  slice(-c(388, 525, 532, 324, 325, 326))%>% #excluding problematic rows for now
  mutate(
    prop_EcM_woody = if_else(
      (AM_woody + EcM_woody + Dual_woody + ErM_woody +NM_woody) == 0,
      NA_real_,
      (EcM_woody + 0.5 * Dual_woody) /
        (AM_woody + EcM_woody + Dual_woody+ ErM_woody +NM_woody)
    )
  )%>%
  mutate(
    prop_AM_woody = if_else(
      (AM_woody + EcM_woody + Dual_woody + ErM_woody +NM_woody) == 0,
      NA_real_,
      (AM_woody + 0.5 * Dual_woody) /
        (AM_woody + EcM_woody + Dual_woody+ ErM_woody +NM_woody)
    )
  )%>%
  mutate(
    prop_ErM_woody = if_else(
      (AM_woody + EcM_woody + Dual_woody + ErM_woody +NM_woody) == 0,
      NA_real_,
      (ErM_woody) /
        (AM_woody + EcM_woody + Dual_woody+ ErM_woody +NM_woody)
    )
  )%>%
  mutate(
    dominant_myco = case_when(
      prop_EcM_woody > 0.5 ~ "EcM",
      prop_AM_woody  > 0.5 ~ "AM",
      prop_ErM_woody > 0.5 ~ "ErM",
      TRUE ~ NA_character_
    )
  )%>%
  mutate(
    fire_dependent = case_when(
      fri_surfac <= 25 ~ "fire-adapted",
      fri_surfac  > 25 ~ "fire-sensitive",
      TRUE ~ NA_character_
    ))%>%
  mutate(
    myco_fire_group = paste(dominant_myco, fire_dependent, sep = "_")
  )
  
write.csv(bps_scls_summary, file = "outputs/bps_scls_summary.csv", row.names = FALSE )

#check whether there are any rows with no woody assignments
bps_scls_summary %>%
  filter(
    AM_woody == 0,
    Dual_woody == 0,
    EcM_woody == 0,
    AM_herbaceous == 0,
    EcM_herbaceous == 0,
  )

#make histograms of the data
ggplot(bps_scls_summary, aes(x = fri_surfac)) +
  geom_histogram(bins = 30) +
  xlab("Fire Return Interval") +
  ylab("Count") +
  theme_bw()

ggplot(bps_scls_summary, aes(x = prop_EcM_woody)) +
  geom_histogram(bins = 30) +
  xlab("Proportion EcM Woody") +
  ylab("Count") +
  theme_bw()

##initial data exploration only 1-1000 FRI
EcMgraph <- bps_scls_summary %>%
  filter(fri_allfir <= 1500) %>%
  ggplot(aes(x = fri_allfir, y = prop_EcM_woody)
) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    color = "black"
  ) +
  xlab("Fire frequency") +
  ylab("Proportion EcM woody") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )

#EcM to FRI linear only 1-1000 FRI
EcMgraph <- bps_scls_summary %>%
  filter(fri_allfir <= 1500) %>%
  ggplot(aes(x = fri_allfir, y = prop_EcM_woody)
  ) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  xlab("Fire Return Interval") +
  ylab("Proportion EcM woody") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )

##AM to FRI linear o
AMgraph <- bps_scls_summary %>%
  filter(fri_surfac <= 2000,
         groupveg != "Riparian") %>%
  ggplot(aes(x = fri_surfac, y = prop_AM_woody)
  ) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  xlab("Fire Return Interval") +
  ylab("Proportion AM woody") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )

##EcM to FRI linear o
EcMgraph <- bps_scls_summary %>%
  filter(fri_surfac <= 100, groupveg != "Riparian") %>%
  ggplot(aes(x = fri_surfac, y = prop_EcM_woody)
  ) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  xlab("Fire Return Interval") +
  ylab("Proportion EcM woody") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )

##ErM to FRI linear o
ErMgraph <- bps_scls_summary %>%
  filter(fri_surfac <= 2000) %>%
  ggplot(aes(x = fri_surfac, y = prop_ErM_woody)
  ) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  xlab("Fire Return Interval") +
  ylab("Proportion ErM woody") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )

#initial analyses... significant relationship!
lmer1 <-lmer(sqrt(prop_EcM_woody + 0.001) ~ fri_surfac+(1|bps_model), data = bps_scls_summary)
anova(lmer1)

#check normality of residuals using qq plot. plots should fall roughly along the line if residuals are normal. They do fall roughly along the line!
qqnorm(residuals(lmer1))
qqline(residuals(lmer1))

#p value is 0.6 so the residuals are likely normally distributed. USE THIS MODEL
shapiro.test(residuals(lmer1))


#initial analyses... nothing significant so far for all fires and replacement fires 
lmer1 <-lmer(sqrt(prop_EcM_woody) ~ fri_allfir+(1|bps_model), data = bps_scls_summary)
anova(lmer1)

#check normality of residuals using qq plot. plots should fall roughly along the line if residuals are normal. They do fall roughly along the line!
qqnorm(residuals(lmer1))
qqline(residuals(lmer1))

#p value is 0.6 so the residuals are likely normally distributed
shapiro.test(residuals(lmer1))


#change in EcM over time
EcMgraph <- bps_scls_summary %>%
  ggplot(aes(x = prop_EcM_woody, y = class_percent_difference)
  ) +
  geom_point(size = 2.5, alpha = 0.6) +
  geom_smooth(
    method = "lm",
    color = "black"
  ) +
  xlab("Proportion EcM woody") +
  ylab("Percent difference") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 11, colour = "black"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    panel.grid = element_blank()
  )




#Make summary plot for generating errorbars
bps_scls_summary_bar<-bps_scls_summary%>%
  dplyr::group_by(dominant_myco) %>%
  dplyr::summarise(perc_avg=mean_se(class_percent_difference)) 

bps_scls_summary_bar["std.error"] <- bps_scls_summary_bar$perc_avg$ymax - bps_scls_summary_bar$perc_avg$ymin

Graph <- ggplot(filter(bps_scls_summary, dominant_myco %in% c("AM", "EcM")),
) + 
  geom_errorbar(bps_scls_summary_bar, 
                mapping=aes(ymin = perc_avg$ymin, ymax = perc_avg$ymax, x=dominant_myco, 
                            color=dominant_myco), position= "dodge", width=.75, size=0.75)+
  geom_point(bps_scls_summary, mapping=aes(x= dominant_myco, y = class_percent_difference, color = dominant_myco), 
             size = 2.5, alpha = 0.2, position= position_dodge(width=0.75))+
  geom_point(data = bps_scls_summary_bar, mapping=aes(x=dominant_myco, y= perc_avg$y , color=dominant_myco), size = 2.5, position= position_dodge(width=0.75))+
  xlab("Dominant Mycorrhizal Type") +
  ylab("Percent Change") +
  scale_color_manual(values=c("#009E73", "#CC79A7", "#E69F00")) + 
  theme_bw() +
  guides(fill = guide_legend(title="Treatment:", reverse = TRUE)) +
  theme(axis.text.x=element_text(size=13, colour="black", vjust=0.5),
        axis.text.y=element_text(size=11, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, colour="black"),
        legend.position="top",
        panel.grid=element_blank())


#Make summary plot for generating errorbars
bps_scls_summary_bar<-bps_scls_summary%>%
  dplyr::group_by(dominant_myco, fire_dependent) %>%
  dplyr::summarise(perc_avg=mean_se(class_percent_difference))%>%
  filter(dominant_myco %in% c("AM", "EcM"))

bps_scls_summary_bar["std.error"] <- bps_scls_summary_bar$perc_avg$ymax - bps_scls_summary_bar$perc_avg$ymin

Graph <- ggplot(filter(bps_scls_summary, dominant_myco %in% c("AM", "EcM")),
) + 
  geom_errorbar(bps_scls_summary_bar, 
                mapping=aes(ymin = perc_avg$ymin, ymax = perc_avg$ymax, x=dominant_myco, 
                            color=fire_dependent), position= "dodge", width=.75, size=0.75)+
  geom_point(bps_scls_summary, mapping=aes(x= dominant_myco, y = class_percent_difference, color = fire_dependent), 
             size = 2.5, alpha = 0.2, position= position_dodge(width=0.75))+
  geom_point(data = bps_scls_summary_bar, mapping=aes(x=dominant_myco, y= perc_avg$y , color=fire_dependent), size = 2.5, position= position_dodge(width=0.75))+
  xlab("Dominant Mycorrhizal Type") +
  ylab("Percent Change") +
  scale_color_manual(values=c("#009E73", "#CC79A7", "#E69F00")) + 
  theme_bw() +
  guides(fill = guide_legend(title="Treatment:", reverse = TRUE)) +
  theme(axis.text.x=element_text(size=13, colour="black", vjust=0.5),
        axis.text.y=element_text(size=11, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=13, colour="black"),
        legend.position="top",
        panel.grid=element_blank())

#see the number of fire-adapted AM and EcM plants
plot_dat <- bps_scls_summary %>%
  filter(dominant_myco %in% c("AM", "EcM")) %>%
  count(dominant_myco, fire_dependent)

#plot it!
ggplot(plot_dat,
       aes(x = dominant_myco,
           y = n,
           fill = fire_dependent)) +
  geom_col(position = "dodge") +
  xlab("Dominant mycorrhizal type") +
  ylab("Number of forest types") +
  labs(fill = "Fire adaptation") +
  theme_bw()
