library(worrms)
library(readr)
library(rfishbase)
library(dplyr)

setwd("~/Dropbox/Rasher Lab/b_NSF EPSCoR Maine eDNA/species references/Fish")

##SpeciesList_UpdatedNames is the document returned from worms when checking accepted names
##This list is the "complete" list tab from the FISH_list document, with 50 inverts removed
fishfriends <- read_csv("SpeciesList_UpdatedNames.csv")

fishfriends <- fishfriends %>% select(ScientificName_accepted, `Phylum/Division`, Class, Order, Family)

fish <- fishfriends %>% filter(Class == "Actinopterygii" |
                               Class == "Elasmobranchii")

write_csv(fish, "FishSpeciesOfInterest.csv")
fish <- read_csv("FishSpeciesOfInterest.csv")

friends <- fishfriends %>% filter(Class != "Actinopterygii" &
                                  Class != "Elasmobranchii")

write_csv(friends, "NonFishSpeciesOfInterest.csv")
friends <- read_csv("NonFishSpeciesOfInterest.csv")


#get all fish by country, fishbase
fishcountry <- country(fish$ScientificName_accepted)
#get all fish by distribution, fishbase
fishdist <- distribution(fish$ScientificName_accepted)
#get all fish by ecosystem, fishbase
fisheco <- ecosystem(fish$ScientificName_accepted)

##Paired Down
fishcountry2 <- fishcountry %>% select(Species, country)
fishdist2 <- fishdist %>% select(Species, FAO)
fisheco2 <- fisheco %>% select(Species, EcosystemName)

#factor to look at NAs and test to see how many species have no distribution
fishdist2$FAO <- as.factor(fishdist2$FAO)
NAFishDist <- fishdist2 %>% filter(is.na(FAO))

#factor to look at NAs and test to see how many species have no distribution
fishcountry2$country <- as.factor(fishcountry2$country)
NAFishCountry <- fishcountry2 %>% filter(is.na(country))

#factor to look at NAs and test to see how many species have no distribution
fisheco2$EcosystemName <- as.factor(fisheco2$EcosystemName)
NAFishEco <- fisheco2 %>% filter(is.na(EcosystemName))

#FIlter by ocean distribution
fishDistAtlantic <- fishdist2 %>% filter(FAO == "Atlantic, Northwest" | 
                                     FAO == "Atlantic, Western Central" |
                                     FAO == "America, North - Inland waters" |
                                     FAO == "Arctic Ocean")

#Filter by country
fishCountryUSA <- fishcountry2 %>% filter(country == "USA" | 
                                           country == "Canada" |
                                           country == "Greenland")

#Filter by ecosystem
fishEcoAtlantic <- fisheco2 %>% filter(EcosystemName == "Arctic" | 
                                  EcosystemName == "Arctic Complex" |
                                  EcosystemName == "Atlantic Complex" |
                                  EcosystemName == "Atlantic Ocean" |
                                  EcosystemName == "Canadian Eastern Arctic - West Greenland" |
                                  EcosystemName == "Canadian High Arctic - North Greenland" |
                                  EcosystemName == "Chesapeake Bay" |
                                  EcosystemName == "Gulf of Maine" |
                                  EcosystemName == "Hudson Bay Complex" |
                                  EcosystemName == "Hudson Complex" |
                                  EcosystemName == "Labrador - Newfoundland"  |
                                  EcosystemName == "Northeast U.S. Continental Shelf" |
                                    EcosystemName == "St. Lawrence Complex"  |
                                    EcosystemName == "St. Lawrence"  |
                                    EcosystemName ==  "West Greenland Shelf")


#Get unique entries for filtered data

fishDistFilterUnique <- fishDistAtlantic %>% select (-FAO) %>% distinct()

fishEcoFilterUnique <- fishEcoAtlantic %>% select (-EcosystemName) %>% distinct()

fishCountryFilterUnique <- fishCountryUSA %>% select (-country) %>% distinct()

FishWeWant <- rbind(fishDistFilterUnique, fishEcoFilterUnique, fishCountryFilterUnique)

FishWeWant <- FishWeWant %>% distinct()

write_csv(FishWeWant, "C:/Users/renef/Dropbox/Rasher Lab/b_NSF EPSCoR Maine eDNA/species references/Fish/FishSpeciesOfInterest_Region.csv")
FishWeWant <- read_csv("FishSpeciesOfInterest_Region.csv")

#### Try to obtain all records for these species

library(rentrez)
library(stringr)
library(readr)
library(dplyr)

##Read in csv that has species of interest in atlantic region, 634 species
FishWeWant <- read_csv("FishSpeciesOfInterest_Region.csv")

FishWeWant_Voucher <- FishWeWant %>% mutate(SearchTerm = paste0( Species,"[ORGN] AND 12S[ALL] AND voucher[ALL]"),
                                     Count = NA,
                                     GeneIDs = NA,
                                     Fasta = NA)

set_entrez_key("ca1d6d632019442c0ffd88c8f37d816fad08")

for(i in 1:nrow(FishWeWant_Voucher)){
  #initial search for each fish on the list
  SearchResult <- rentrez::entrez_search(db = "nucleotide", 
                                         term = FishWeWant_Voucher$SearchTerm[i])
  
  FishWeWant_Voucher$Count[i] <- SearchResult$count
  
  FishWeWant_Voucher$GeneIDs[i] <- toString(SearchResult$ids)
  
  #add fasta files for all geneids that have fastas, initially still has "\n" in the strings
  if(FishWeWant_Voucher$Count[i] > 0) {
  FishWeWant_Voucher$Fasta[i] <- rentrez::entrez_fetch("nucleotide", id = SearchResult$ids, rettype = "fasta", retmode = "text")
  }
  
  #remove "\n" from fasta
  FishWeWant_Voucher$Fasta[i] <- str_replace_all(FishWeWant_Voucher$Fasta[i], "[\r\n]", "")
  
}


#includes counts of how many entries there are for each species when searching species and 12s AND voucher
#write_csv(FishWeWant_Voucher, "FishWeWant_Voucher.csv") ##Original Version July 2023

write_csv(FishWeWant_Voucher, "FishWeWant_Voucher_October.csv") ##New Version October 2023

#FishWeWant_Voucher <-read_csv("FishWeWant_Voucher.csv") 

FishWeWant_Voucher <-read_csv("FishWeWant_Voucher_October.csv") 

#249 fish dont have vouchers
FishWeWant_NoVoucher <- FishWeWant_Voucher %>% filter(Count == 0)

FishWeWant_NoVoucher <- FishWeWant_NoVoucher %>% mutate(SearchTerm = paste0(Species,"[ORGN] AND 12S[ALL]"))

#look at those that dont have vouchers
for(i in 1:nrow(FishWeWant_NoVoucher)){
  #initial search for each fish on the list
  SearchResult <- rentrez::entrez_search(db = "nucleotide", 
                                         term = FishWeWant_NoVoucher$SearchTerm[i])
  
  FishWeWant_NoVoucher$Count[i] <- SearchResult$count
  FishWeWant_NoVoucher$GeneIDs[i] <- toString(SearchResult$ids)

  #add fasta files for all geneids that have fastas, initially still has "\n" in the strings
  if(FishWeWant_NoVoucher$Count[i] > 0) {
    FishWeWant_NoVoucher$Fasta[i] <- rentrez::entrez_fetch("nucleotide", id = SearchResult$ids, rettype = "fasta", retmode = "text")
  }

  #remove "\n" from fasta
  FishWeWant_NoVoucher$Fasta[i] <- str_replace_all(FishWeWant_NoVoucher$Fasta[i], "[\r\n]", "")
}


#includes counts of how many entries there are for each species when searching species and 12s AND voucher
# 249 have no voucher but could potentially be in serc database

#write_csv(FishWeWant_NoVoucher, "FishWeWant_NoVoucher.csv")  ##Original Version July 2023

write_csv(FishWeWant_NoVoucher, "FishWeWant_NoVoucher_October.csv") ##New Version October 2023

#FishWeWant_NoVoucher <-read_csv("FishWeWant_NoVoucher.csv") 

FishWeWant_NoVoucher <-read_csv("FishWeWant_NoVoucher_October.csv") 


###Create list of any species without vouchers to Katie
FishWeWant_Katie <- FishWeWant_NoVoucher %>% dplyr::select(-Fasta)

write_csv(FishWeWant_Katie, "FishWeWant_Kate.csv")

############ Compare ones without voucher to smithsonian with bulk of fasta row

NoVoucherNF <- FishWeWant_NoVoucher %>% dplyr::select(-Fasta)

#read in SERC file
SERC <- read_csv("MifishPlus_SERC_SpeciesNames.csv")

#look only at species, only each one once to allow for easy comparison
SERCSpecies <- SERC %>% 
  mutate(SERC_Species = paste(Genus, Species)) %>%
  dplyr::select(SERC_Species) %>% rename("Species" = "SERC_Species") %>% distinct()

  
###compare the SERCSpecies with the Species column of NoVoucherNF
SERC_Want <- inner_join(NoVoucherNF, SERCSpecies, by = "Species")

## 71 individuals without vouchers have data in the SERC database of the 249 that dont have vouchers
SERC_Want <- SERC_Want %>% dplyr::select(Species)

#write_csv(SERC_Want, "FishWeWant_SERC.csv") #Original Version July 2023  

write_csv(SERC_Want, "FishWeWant_SERC_October.csv")  ##New Version October 2023
  
  
### remove the species from the NoVoucher list that are in the SERC database  

SERC_Want <- SERC_Want %>% mutate(Source = "SERC") 
  
FishWeWant_NoVoucher_NoSERC <- full_join(FishWeWant_NoVoucher, SERC_Want, by = "Species")

FishWeWant_NoVoucher_NoSERC <- FishWeWant_NoVoucher_NoSERC %>% filter(is.na(Source)) %>% dplyr::select(-Source)

## 178 individuals with no voucher and not in the SERC database
#write_csv(FishWeWant_NoVoucher_NoSERC, "FishWeWant_NoVoucher_NoSERC.csv")  ##Original Version July 2023

write_csv(FishWeWant_NoVoucher_NoSERC, "FishWeWant_NoVoucher_NoSERC_October.csv") ##Current version October 2023


# 156 have no references available to us :( 
NoData <- FishWeWant_NoVoucher_NoSERC %>% filter(Count == 0)
#write_csv(NoData, "FishWeWant_NoData.csv") #Original Verison July 2023

write_csv(NoData, "FishWeWant_NoData_October.csv") # New version October 2023


##################################################
## Try to decipher if it can be paired down so its not laborious to look through


FishWeWant_Voucher <-read_csv("FishWeWant_Voucher_October.csv") 

FishWeWant_VoucherOnly <- FishWeWant_Voucher %>% filter(Count != 0)

write_csv(FishWeWant_VoucherOnly, "FishWeWant_VoucherOnly.csv")

FishWeWant_VoucherOnly <- read_csv("FishWeWant_VoucherOnly.csv")

##use Voucher Only as a list to search for individuals with KYWD "refseq" to remove duplicates?

FishWeWant_Refseq <- FishWeWant_VoucherOnly %>% 
  rename(Count_Original = Count) %>%
  rename(GeneIDs_Original = GeneIDs) %>%
  #rename(Fasta_Original = Fasta) %>%
  mutate(SearchTerm = paste0(SearchTerm,"AND Refseq [KYWD]"),
                                            Count = NA,
                                            GeneIDs = NA,
                                            Fasta = NA)

for(i in 1:nrow(FishWeWant_Refseq)){
  #initial search for each fish on the list
  SearchResult <- rentrez::entrez_search(db = "nucleotide", 
                                         term = FishWeWant_Refseq$SearchTerm[i])
  
  FishWeWant_Refseq$Count[i] <- SearchResult$count
  
  FishWeWant_Refseq$GeneIDs[i] <- toString(SearchResult$ids)
  
  #add fasta files for all geneids that have fastas, initially still has "\n" in the strings
  if(FishWeWant_Refseq$Count[i] > 0) {
    FishWeWant_Refseq$Fasta[i] <- rentrez::entrez_fetch("nucleotide", id = SearchResult$ids, rettype = "fasta", retmode = "text")
  }
  
  #remove "\n" from fasta
  FishWeWant_Refseq$Fasta[i] <- str_replace_all(FishWeWant_Refseq$Fasta[i], "[\r\n]", "")
  
}


##FishWeWant Refseq narrows some entries down by limiting to NC or similar references
#no great way to identify NC samples but adding "RefSeq" as Keyword seems to have worked?

#Filter out ones without refseq input
FishWeWant_RefseqOnly <- FishWeWant_Refseq %>% 
  na.omit() %>%
  dplyr::select(-Count_Original,-GeneIDs_Original)


#filter out ones so that they are only non-refseq
FishWeWant_RefseqOnly <- FishWeWant_RefseqOnly %>%
  mutate(Category = "Refseq")

FishWeWant_RefseqNo <- FishWeWant_Refseq %>%
  filter(Count == 0) %>%
  dplyr::select(-Count, -GeneIDs) %>%
  rename(Count = Count_Original) %>%
  rename(GeneIDs = GeneIDs_Original) %>%
  mutate(Category = "Voucher")
  
###merge togehter with 86 having less GeneIDs to go though
##get original fasta entries for vouchered samples
VoucherFasta <- FishWeWant_VoucherOnly %>% dplyr::select(Species, Fasta)

VoucherOnlyFasta <- inner_join(FishWeWant_RefseqNo, VoucherFasta, by = "Species")

VoucherOnlyFasta <- VoucherOnlyFasta %>% dplyr::select(-Fasta.x) %>%
  rename(Fasta = Fasta.y)

FishWeWant_RefSeq_Voucher <- rbind(FishWeWant_RefseqOnly, VoucherOnlyFasta) %>% arrange(Species)

write_csv(FishWeWant_RefSeq_Voucher, "FishWeWant_RefSeq_Voucher.csv")

FishWeWant_RefSeq_Voucher <- read_csv("FishWeWant_RefSeq_Voucher.csv")

### number of geneids for each species
#1    2   3   4   5    6   7   8   9   10  12  13  14  15  17 27  40  47  57  58 
#234  91  54  22  15   6   6   6   2   4   3   1   1   2   1  1   1   1   2   1 





