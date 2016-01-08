import sys, calendar, csv
from urllib2 import urlopen, URLError
from argparse import ArgumentParser
from bs4 import BeautifulSoup


def parse_arguments():
    """ Process command line arguments """
    parser = ArgumentParser(description='Grabs tables from html')
    parser.add_argument('-m', '--month', help='month', required=True)
    parser.add_argument('-y', '--year', help='year', required=True)
    args = parser.parse_args()
    return args

def main():
    # Get arguments
    args = parse_arguments()
    if args.month:
        month = args.month
    if args.year:
        year = args.year

    url = 'http://www.comichron.com/monthlycomicssales/'
    url = url + year + '/' + year + '-' + month + '.html'
    print url
    # Make soup
    try:
        resp = urlopen(url)
    except URLError as e:
        print 'An error occured fetching %s \n %s' % (url, e.reason)   
        return 1
    soup = BeautifulSoup(resp.read(), "html.parser")

    # Get rows

    tables = soup.find_all('table')

    # This will get the first (and only) table. Your page may have more.
    my_table = tables[1]

    pr_rank = 0
    frmt = "COMIC"

    with open("./month_sales/" + year + "_" + calendar.month_name[int(month)].upper() + ".csv", "w") as file:
        csv_file = csv.writer(file)

        # You caxsn find children with multiple tags by passing a list of strings
        rows = my_table.findChildren(['tr'])
        for row in rows:
            cells = row.findChildren('td')
            data = {}
            if (len(cells)==6):
                if len(cells[0].get_text().strip()) > 0:
                    if (int(cells[0].get_text().strip()) < pr_rank):
                        frmt = "TPB"
                    data["format"] = frmt
                    data["year"] = year
                    data["month"] = calendar.month_name[int(month)].upper()
                    data["mrank"] = int(cells[0].get_text().strip())
                    data["title"] = cells[1].get_text().strip()
                    if isinstance(data["title"], unicode):
                        data["title"] = data["title"].encode('UTF-8')
                    
                    data["issue"] = cells[2].get_text().strip()
                    
                    if len(cells[3].get_text().strip()) > 0:
                        data["price"] = float(cells[3].get_text().replace("$","").strip())
                    else:
                        data["price"] = ''
                    
                    data["publisher"] = str(cells[4].get_text().strip())
                    data["est_sales"] = int(cells[5].get_text().replace(",","").strip())
                    pr_rank = int(cells[0].get_text().strip())
                    csv_file.writerow([data["format"],data["year"],data["month"],data["mrank"],data["title"],data["issue"],data["price"],data["publisher"],data["est_sales"]])

if __name__ == '__main__':
    status = main()
    sys.exit(status)

