import io
import re
import datetime

#Sort them so they are in chronological order by MRN, reverseChrono since needs to next value inputted
initial = open("justText.csv", 'r')
new = open("PhoneNumbersInText.csv", 'w')



linecount = 0

#allWords = {}

for aline in initial:
    line = re.sub('\t','',aline.upper())

    linecount += 1

    FromPagingOffice = "Please do not reply to this message.".upper()

    if linecount % 1000 == 0:
        print linecount
        
    if linecount > 1 :


        number = ""


        if  len(re.findall(FromPagingOffice, line)) >= 0:

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
                    
            
        new.write(str(linecount) + "," + number + "\n")

        #print allWords

    else:
        new.write(str(linecount) + ",PhoneNumbers\n")

initial.close()
new.close()


print "eval all lines done"

#dictionaryWords = open("WordsInTextDictionaryWithIncidence.csv", 'w')

#for items in allWords.keys():
#    dictionaryWords.write(items + "," + str(allWords[items]) + '\n')

#dictionaryWords.close()

