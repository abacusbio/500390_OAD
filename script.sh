cd /e/500390_OAD/explore

##-- break contemporaryGroup column into 6 meaningful columns --#$

# print field 2, print 2:n rows, use - as the dilimitor, print all fields
# takes long time
awk -F',' '{print $2}' ../../OAD\ Dairy\ Data/ProductionExtract.csv | tail -n +2 | awk -F'-' '{print $1, $2, $3, $4, $5, $6}' > contGroup
wc -l contGroup # 248128748
echo "HerdDurableKey DairyYear Season EventDate AgeParityGroup RegimeType" > header
cat header contGroup > cont_group
wc -l cont_group # 248,128,749
rm contGroup
# print every field without field 2
awk -F',' '{sec=$2; $2=""; print $0; sec;}' ../../OAD\ Dairy\ Data/ProductionExtract.csv > tmp
paste tmp cont_group -d' ' > test # dilimiter changed to space
awk -F' ' '{print NF; exit}' test # count columns
tail test # sanity check
head test # sanity check
mv test productionextract_original  # 248,128,749
rm tmp cont_group header

###-- clean data --###

head -n 1 productionextract_original > header
##--delete animals entered the heard before 2007/2008 season##
##--2007 June 1st--##
# awk -F' ' '{ if ($12 < 20070601) {print}}' productionextract |sort -u > tmp
# wc -l tmp # 12424779 animals to be eliminated
# grep -vFf tmp productionextract > test # reverse match, fixed pattern, file tmp
# wc -l test # 107933032 records left
# awk '{print $9}' test |sort -u |wc -l # 17972 herds left
# mv test productionextract

##--remove test records before 2006 (DairyYear)--##
awk '{print $10}' productionextract_original |sort -u # 2007 - 2021
awk '{ if ($10 >= 2007) { print } }' productionextract_original > tmp
awk '{ if ($10 < 2007) print }' tmp |wc -l # saniy check
awk '{print $10}' tmp | sort -u # sanity check
head tmp # sanity check
tail tmp # sanity check
wc -l tmp # 107910530 
awk '{ if ($10 < 2007) { print } }' productionextract_orignal |wc -l # 6335115
awk '{ if ($10="") { print } }' productionextract_original |wc -l # 0
# sed 's/\t/ /g' tmp
mv tmp productionextract

# get birthday
# test day - daysfromparturitiontoherdtest, getline d means save the new var as 'd'
# test day, birth-to-par, par-to-test, new date
##awk -F' ' '(NR>1){ ("date +%Y%m%d -d" $12 "-" $3 "days" |getline d); print $12, $2, $3, d}' productionextract >test
# d - daysfrombirthtoparturition
# test day, birth-to-par, par-to-test, new date, birthday
##awk -F' ' '(NR>1){ ("date +%Y%m%d -d" $4 "-" $2 "days" |getline d); print $1, $2, $3, $4, d}' test >tmp


###-- R computing in between --###

###-- sort by AnimalDurableCode, event_date --###

sort -c ../output/animal_herd_eventDate.csv # sanity check
tail -n +2 ../output/animal_herd_eventDate.csv > tmp
head -n 1 ../output/animal_herd_eventDate.csv >header
sort -k2,2n -k1,1 -k3,3 -t',' tmp > tmp_sorted
sort -c tmp # sanity check
cat header tmp > animal_herd_eventDate_herd_sorted
# sed 's/ /,/g' tmp > animal_eventDate_herd_sorted

