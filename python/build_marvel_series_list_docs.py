import urllib2, json, time, hashlib, csv, traceback

debug_lvl = 3

def debug (lvl, txt):
    if ( debug_lvl>=lvl):
        print txt

def getSeriesByLetter(letter,activeYear, order=''):
    m = hashlib.md5()
    limit='100'
    ts = str(time.time())
    m.update(ts + '0265f278e4edec8cbc24428879b260ee8673c0b0b97081f95e4173711d6d3d0915dfcde9')
    n = m.hexdigest()
    nameFilter = letter
    orderBy = order + 'title'
    seriesUri = 'http://gateway.marvel.com/v1/public/series?apikey=b97081f95e4173711d6d3d0915dfcde9&hash=' + n + '&ts=' + ts
    seriesUri = seriesUri + '&limit=' + limit + '&titleStartsWith=' + nameFilter+ '&modifiedSince=' + activeYear + '-01-01&contains=comic&&orderBy=' + orderBy
    debug(3, seriesUri)
    seriesDataWrapper = json.loads(urllib2.urlopen(seriesUri).read())

    seriesDataContainer =  (seriesDataWrapper["data"])
    debug(2, letter + " total:" + str(seriesDataContainer["count"]) + "/" + str(seriesDataContainer["total"]))
    debug(3, "limit:" + str(seriesDataContainer["limit"]))
    results =  seriesDataContainer["results"]
    return results

def writeSeriesToCsvByLetter(file_path,series):
    with open(file_path, "a") as file:
        csv_file = csv.writer(file)
        for i in range(len(series)):
            title =  series[i]

            rating = title["rating"]
            if isinstance(rating, unicode):
                rating = rating.encode('UTF-8')

            name = title["title"]
            if isinstance(name, unicode):
                name = name.encode('UTF-8')
    
            csv_file.writerow([title['id'], name, title["startYear"],title["endYear"],rating])
        
    

for i in range(ord('a'), ord('z')+1):
    letter = chr(i)
    writeSeriesToCsvByLetter('./MarvelSeriesLists/MarvelSeriesList_' + letter + '.csv', getSeriesByLetter(letter,'2014'))

for i in range(1,9+1):
    letter = str(i)
    writeSeriesToCsvByLetter('./MarvelSeriesLists/MarvelSeriesList_' + letter + '.csv', getSeriesByLetter(letter,'2014'))

# Reverse sort Used to overcome Marvel's 100 result limit. will need to de-dep because of overlap
writeSeriesToCsvByLetter('./MarvelSeriesLists/MarvelSeriesList_st.csv', getSeriesByLetter('st','2014'))
writeSeriesToCsvByLetter('./MarvelSeriesLists/MarvelSeriesList_st_rev.csv', getSeriesByLetter('st','2014','-'))
writeSeriesToCsvByLetter('./MarvelSeriesLists/MarvelSeriesList_s_rev.csv', getSeriesByLetter('s','2014','-'))
