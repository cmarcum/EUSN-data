wget http://www.eusn2017.uni-mainz.de/files/2016/08/EUSN2017_Booklet_25_09.pdf

pdftk EUSN2017_Booklet_25_09.pdf cat 19-63 output EUSN.pdf

pdftotext EUSN.pdf

#First the coauthor only network
#ELiminate some lines
grep "Session" EUSN.txt -v > EUSN2.txt

#Manually curate the rest
#gedit EUSN2.txt

#elimate superscripts
sed 's/[[:digit:]]//g' EUSN2.txt > EUSN3.txt

#properly encode from unknown
iconv -c -t UTF-8 EUSN3.txt > EUSN4.txt
#verify success
file -i EUSN4.txt

#Then EUSN4.txt was converted to csv via oocalc : eusn.manual.csv

#Now do the organizer network
grep "Session Org*" EUSN.txt -A 1 > EUSNorg.txt
grep "Session Chair" EUSNorg.txt -v > EUSNorg2.txt

#Hand curation step on EUSNorg2.txt to remove erratic carriage returns, also corrected one period

#Remove first column
sed 's/Session Organizer(s): //g' EUSNorg2.txt > EUSNorg3.txt

#remove aesthetic delimiter
grep "\-\-" EUSNorg3.txt -v > EUSNorg4.txt

#Then EUSNorg4.txt was converted to csv via oocalc : eusnorg.manual.csv
