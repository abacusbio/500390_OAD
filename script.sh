cd /e/explore

##-- break contemporaryGroup column into 6 meaningful columns --#$

# print field 2, print 2:n rows, use - as the dilimitor, print all fields
# takes long time
awk -F',' '{print $2}' ../OAD\ Dairy\ Data/ProductionExtract.csv | tail -n +2 | awk -F'-' '{print $1, $2, $3, $4, $5, $6}' > contGroup
wc -l contGroup # 248128748
echo "HerdDurableKey DairyYear Season EventDate AgeParityGroup RegimeType" > header
cat header contGroup > cont_group
wc -l cont_group # 248128749
rm contGroup
# print every field without field 2
awk -F',' '{sec=$2; $2=""; print $0; sec;}' ../OAD\ Dairy\ Data/ProductionExtract.csv > tmp
paste tmp cont_group > test # dilimiter changed to space
awk -F' ' '{print NF; exit}' test # count columns
mv test productionextract
rm tmp cont_group header

##-- clean data --##