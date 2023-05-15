for f in $(<listNCT.txt)
do
mongoimport --db aci --collection ClinicalTrials --type json --file /path/to/trialsfolder/$f --jsonArray
done
