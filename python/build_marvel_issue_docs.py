import sys, urllib, urllib2, json, re, hashlib, traceback, csv, os.path, time, re
from time import strptime
from fuzzywuzzy import fuzz, process


reload(sys)  
sys.setdefaultencoding('Cp1252')


debug_lvl = 3
def debug (lvl, txt):
    if ( debug_lvl>=lvl):
        print txt



def wordSwap(s):
    d = {
        'shield':'S.H.I.E.L.D',
        'modok':'M.O.D.O.K'
        }
    pattern = re.compile(r'\b(' + '|'.join(d.keys()) + r')\b',re.IGNORECASE)
    return pattern.sub(lambda x: d[x.group().lower()], s)

def mapTitle(title, pub_year):
    
    mtitles = []
    with open('/Users/petertamisin/Github/comic-predictions/data/marvel_series_list.csv', 'rb') as csvfile:
        reader = csv.DictReader( csvfile )
        for line in reader:
            if (int(pub_year) >= int(line[ 'startyear' ]) and int(pub_year) <= int(line[ 'endyear' ])):        
                debug(4,line)
                # Remove Series Dates from title
                if (line['title'].endswith(')') and line['title'].rfind('(')>0) :
                    idx = line['title'].rfind('(')    
                    line['newtitle'] = line['title'][0:idx]
                    line['newtitle'] = line['newtitle'].rstrip() 
                debug(4, line['newtitle'])
                mtitles.append(line)

    match = (process.extractOne(title, sorted(set(d['newtitle'] for d in mtitles), reverse=True)))
    score = match[1]
    if score >= 90:
        for mline in mtitles:
            if(mline["newtitle"] == match[0]):
                debug(4,mline)
                return mline
    else:
        #try match again with translated title
        match = (process.extractOne(wordSwap(title), sorted(set(d['newtitle'] for d in mtitles), reverse=True)))
        score = match[1]
        if score >= 90:
            for mline in mtitles:
                if(mline["newtitle"] == match[0]):
                    debug(4,mline)
                    return mline
        
        debug(2, 'MISMATCH:' + ''.join(str(e) for e in match) + '~' + wordSwap(title))
        return 

def getIssueDetails(title, issue, pub_year, pub_month, est_sales, month_rank):
    debug(3,"Params:" + title + "," + issue + "," + str(pub_year) + "," + str(pub_month) + "," + est_sales + "," + month_rank)
    m = hashlib.md5()
    ts = str(time.time())
#    m.update(ts + '0265f278e4edec8cbc24428879b260ee8673c0b0b97081f95e4173711d6d3d0915dfcde9')
    m.update(ts + '2b5f72b91efe4414ab080cd2d3ccd8af760759d1fafdbb031b1c38bae1c8b940ad11a900')
    n = m.hexdigest()
    limit="100"
    issue = str(issue)
    pub_year = str(pub_year)
    # set range to +- 1 month
    pub_month = strptime(pub_month,'%B').tm_mon
    start_month = str(int(pub_month) -1) if pub_month > 1 else '12'
    start_year = pub_year if pub_month > 1 else str(int(pub_year) - 1)
    end_month = str(int(pub_month) +1) if pub_month < 12 else '1'
    end_year = pub_year if pub_month < 12 else str(int(pub_year) + 1)
    debug(4, title + ' ' + issue + ' ' + str(pub_year) + ' ' + str(pub_month) + ' ' + est_sales + ' ' + month_rank)
    matchedTitle = mapTitle(title, int(pub_year))
    debug(5, matchedTitle)
    if matchedTitle == None:
        return
    
    uriParams = {}
#    uriParams["apikey"] = 'b97081f95e4173711d6d3d0915dfcde9'
    uriParams["apikey"] = 'fafdbb031b1c38bae1c8b940ad11a900'
    uriParams["hash"] = n
    uriParams["ts"] = ts
    uriParams["limit"] = limit
    uriParams["format"] = 'comic'
    uriParams["formatType"] = 'comic'
    uriParams["noVariants"] = 'true'
    uriParams["series"] = matchedTitle['id']  
    #uriParams["titleStartsWith"] = matchedTitle['title'] 
    uriParams["issueNumber"] = issue.replace('*','')
    #uriParams["dateRange"] = start_year + '-' + start_month + '-1,' + end_year + '-' + end_month + '-1'    
    baseUri = 'http://gateway.marvel.com/v1/public/comics?'
    issueUri =  baseUri + urllib.urlencode(uriParams)
    debug(2,title)
    file_path = "/Users/petertamisin/Github/comic-predictions/data/comics/" + re.sub('[^0-9a-zA-Z]+','',title) + "_" + re.sub('[^0-9a-zA-Z]+','',uriParams["issueNumber"]) + "_" + str(pub_year) + "_" + str(pub_month) + ".csv"
    debug(2,file_path)
    if os.path.exists(file_path):
        debug(5, "Skipped:" + file_path)
        return

    if uriParams["series"] == "" or uriParams["issueNumber"] == "":
        debug(2, 'Bad input:' + matchedTitle['title'] + ' (' + matchedTitle['id'] + ') ~ source:' + title + '|Issue:' + uriParams["issueNumber"])
        return
        #raise ValueError('Bad input:' + matchedTitle['title'] + ' (' + matchedTitle['id'] + ') ~ source:' + title + '|Issue:' + uriParams["issueNumber")

    debug(4, issueUri)
    issueDataWrapper = json.loads(urllib2.urlopen(issueUri).read())
    issueDataContainer =  (issueDataWrapper["data"])
    if issueDataContainer["total"] < 1:
        debug(2, 'ZERO RESULTS:' + matchedTitle['title'] + ' (' + matchedTitle['id'] + ') ~ source:' + title + '|Issue:' + uriParams["issueNumber"])
        #raise ValueError("ZERO RESULTS:" + urllib.urlencode(uriParams))
    else:
        issues =  issueDataContainer["results"]
        with open(file_path, "w") as file:
            csv_file = csv.writer(file)
            for i in range(len(issues)):
                issue =  issues[i]
                debug(4, issue)
                # Get Description
                descr = issue["description"]
                if isinstance(descr, unicode):
                    descr.encode('UTF-8')
                
                if descr is not None:
                    descr = descr.replace('\\n', ' ')
                        
                # Get Creators
                creators = issue['creators']['items']
                creatorinfo = ''
                for c in range(len(creators)):
                    creator = creators[c]
                    creatorinfo = creatorinfo + ' | ' + creator["role"] + ':' + creator["name"]
                            
                # Get Characters
                characters = issue['characters']['items']
                characterinfo = ''
                for ch in range(len(characters)):
                    character = characters[ch]
                    characterinfo = characterinfo + ' | ' + character["name"]
                                
                # Get Events
                events = issue['events']['items']
                eventinfo = ''
                for e in range(len(events)):
                    event = events[e]
                    eventinfo = eventinfo + ' | ' + event["name"]
                                    
                # Get Dates
                dates = issue['dates']
                dateinfo = ''
                for d in range(len(dates)):
                    idate = dates[d]
                    dateinfo = dateinfo + ' | ' + idate["type"] + ':' + idate["date"]
                                        
                # Get prices
                prices = issue['prices']
                priceinfo = ''
                for p in range(len(prices)):
                    price = prices[p]
                    priceinfo = priceinfo + ' | ' + price["type"] + ':' + str(price["price"])
                                            
                if(len(issue['title'])>1):                        
                    csv_file.writerow([issue['id'], issue['title'], issue['issueNumber'], est_sales, month_rank, descr, characterinfo, creatorinfo, eventinfo, dateinfo, priceinfo])



for i in os.listdir('/Users/petertamisin/Github/comic-predictions/data/month_sales'):
#    if i.endswith(".csv") and (i.find("2014") > -1): 
    if i.endswith(".csv"): 
        debug(2, i)
        with open('/Users/petertamisin/Github/comic-predictions/data/month_sales/' + i, 'rb') as csvfile:
            reader = csv.DictReader( csvfile )
            for line in reader:
                if (line[ 'PUBLICATION_FORMAT' ] == 'COMIC' and line[ 'PUBLISHER' ] == 'Marvel'):
                    #mapTitle(line['TITLE'],line['YEAR'])
                    getIssueDetails(line[ 'TITLE' ], line[ 'ISSUE' ], line[ 'YEAR' ], line[ 'MONTH' ], line[ 'EST_SALES' ], line[ 'SALES_RANK' ])   
        continue
    else:
        continue
    
