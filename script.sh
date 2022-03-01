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
awk -F',' '{sec=$2; $2=""; print $0; sec;}' ../OAD\ Dairy\ Data/ProductionExtract.csv > tmp
paste tmp cont_group -d' ' > test # dilimiter changed to space
awk -F' ' '{print NF; exit}' test # count columns
tail test # sanity check
head test # sanity check
mv test productionextract  # 248,128,749
rm tmp cont_group header

##-- clean data --##

head -n 1 productionextract > header
# remove test records before 2006 (DairyYear)
awk '{print $10}' productionextract |sort -u # 1986 - 2021
awk '{ if ($10 >= 2006) { print } }' productionextract > tmp
awk '{ if ($10 < 2006) print }' tmp |wc -l # saniy check
awk '{print $10}' tmp | sort -u # sanity check
head tmp # sanity check
tail tmp # sanity check
wc -l tmp # 126,900,184 very few? 248,128,749*16/36=110,279,444
awk '{ if ($10 < 2006) { print } }' productionextract |wc -l # 121,228,565
awk '{ if ($10="") { print } }' productionextract |wc -l # 0
# sed 's/\t/ /g' tmp
mv tmp productionextract

##-- R computing in between --##

##-- sort by AnimalDurableCode, event_date --##

sort -k1,1 -k3,3 -t' ' ../output/animal_herd_eventDate.csv >tmp
sort -c tmp # sanity check
sed 's/ /,/g' tmp > animal_eventDate_herd_sorted

