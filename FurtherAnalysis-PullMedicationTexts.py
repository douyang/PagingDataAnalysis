import io
import re
import datetime

#Sort them so they are in chronological order by MRN, reverseChrono since needs to next value inputted
initial = open("justText.csv", 'r')
new = open("MedicationTextsWithProcessing-Cardiology.csv", 'w')


drugNamesFile = open("DrugNames-Cardiology.csv", 'r')
drugNames = []


drugNamesAndClassificationFile = open("DrugNamesWithClassificationStatistics-Cardiology.csv", 'w')
drugNamesAndClassification = {}

drugNamesAndClassificationFile.write("drugName,totalIncidence,withRNIncidence,withPharmIncidence\n")
#drugName,totalIncidence,withRNIncidence,withPharmIncidence

for line in drugNamesFile:
    med = line.split(',')[0]
    drugNames.append(med.upper().strip())

    drugNamesAndClassification[med.upper().strip()] = [0,0,0]
    
print drugNames

linecount = 0

#allWords = {}

for aline in initial:
    line = re.sub('\t','',aline.upper())

    linecount += 1

    FromPagingOffice = "Please do not reply to this message.".upper()

    if linecount % 1000 == 0:
        print linecount


    drugInPage = 0

    for drug in drugNames:
        if  drug in line:
            drugInPage = 1
            #print drug, line
            break
    
    if linecount > 1 and drugInPage:


        number = ""


        if  FromPagingOffice not in line:

            if (len(re.findall("[0-9]{2}[0-9 ()\-]{2,}[0-9]", line)) > 0):
                number = str(re.findall("[0-9]{2}[0-9 ()\-]{2,}[0-9]", line)).strip("[]' ")

                #print number

        
            #lineWithoutPunc = re.sub("[^a-zA-Z ]", " ", line)
            #lineSplit = lineWithoutPunc.split()
            #
            #for item in lineSplit:
            #    if len(item) > 2 and item in allWords.keys():
            #        allWords[item] += 1

            #    else:
            #        allWords[item] = 1
                    

        descriptor = ""

        if "PHARM" in line:
            descriptor += "PHARMACY"

            #print descriptor,line

        if " RX" in line:
            descriptor += "PHARMACY"

            #print descriptor,line

        if " RN" in line:
            descriptor += "NURSE"

            #print descriptor,line

        if "NURS" in line:
            descriptor += "NURSE"

            #print descriptor,line


        for drug in drugNamesAndClassification:
            if drug in line:
                drugNamesAndClassification[drug][0] += 1

                if "NURSE" in descriptor:
                    drugNamesAndClassification[drug][1] += 1

                if "PHARMACY" in descriptor:
                    drugNamesAndClassification[drug][2] += 1
                    
            
        new.write(line.strip() + "," + descriptor + "," +  number + "\n")

        
    else:
        if linecount == 1:
            new.write(line.strip() + ",Occupation,Phone Numbers\n")

initial.close()
new.close()


print "eval all lines done"

#dictionaryWords = open("WordsInTextDictionaryWithIncidence.csv", 'w')

for items in drugNamesAndClassification.keys():
    drugNamesAndClassificationFile.write(items + "," + str(drugNamesAndClassification[items]).strip('[]') + '\n')

drugNamesAndClassificationFile.close()

