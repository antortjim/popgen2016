SUBSPECIES=(plains chapmani boehmi crawshayi borensis burchelli grevys quagga)

cd ..

for S in ${SUBSPECIES[*]}
do
 echo $S
 plink --chr-set 44 --bfile separate_species/$S/$S --recode --tab --out zebra.maps.peds/$S
 plink --chr-set 44 --noweb --file zebra.maps.peds/$S --freq --out zebra.maps.peds/$S
 # Remove NAs and add species id as last column
 cat zebra.maps.peds/${S}.frq | grep -v NA | sed -e 's/$/ '$S'/' | \
 sed -e '0,/'$S'/{s/'$S'/subspecies/}' >  zebra.maps.peds/${S}_noNA.frq

done


#plink --chr-set 44 --bfile ./separate_species/chapmani/chapmani --recode --tab --out ./zebra.maps.peds/chapmani
#plink --chr-set 44 --bfile ./separate_species/boehmi/boehmi --recode --tab --out ./zebra.maps.peds/boehmi
#plink --chr-set 44 --bfile ./separate_species/crawshayi/crawshayi --recode --tab --out ./zebra.maps.peds/crawshayi
#plink --chr-set 44 --bfile ./separate_species/burchelli/burchelli --recode --tab --out ./zebra.maps.peds/burchelli
#plink --chr-set 44 --bfile ./separate_species/borensis/borensis --recode --tab --out ./zebra.maps.peds/borensis
#
#plink --chr-set 44 --noweb --file chapmani --freq --out  chapmani
#plink --chr-set 44 --noweb --file boehmi --freq --out  boehmi
#plink --chr-set 44 --noweb --file crawshayi --freq --out  crawshayi
#plink --chr-set 44 --noweb --file burchelli --freq --out  burchelli
#plink --chr-set 44 --noweb --file borensis --freq --out  borensis
#
#cat chapmani.frq | grep -v NA > chapmani_noNA.frq
#cat boehmi.frq | grep -v NA > boehmi_noNA.frq
#cat crawshayi.frq | grep -v NA > crawshayi_noNA.frq
#cat burchelli.frq | grep -v NA > burchelli_noNA.frq
#cat borensis.frq | grep -v NA > borensis_noNA.frq
