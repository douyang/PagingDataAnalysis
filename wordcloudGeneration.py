

from os import path
from wordcloud import WordCloud,STOPWORDS
import matplotlib.pyplot as plt

#text = ""
dictionaryWords = open("WordsInTextDictionaryWithIncidence.csv", 'r')
#dictionaryWords = open("WordsInTextDictionaryWithIncidence-OnlyMedications.csv", 'r')


linecount = 0

stopwords = set(STOPWORDS)

dictWords = {}

for line in dictionaryWords:
    if linecount >= 1 and linecount <= 400:
        word = line.split(',')[0].strip()
        freq = line.split(',')[1].strip()

        if len(word) >= 5:
            dictWords[word] = int(freq)

        
#        for i in range(1,int(freq)):
#            text += " " + word
        
    if linecount >= 400:
        break
        
    linecount += 1

    print linecount

    

wordcloud = WordCloud(stopwords = stopwords).generate_from_frequencies(dictWords)

plt.figure()
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.show()
