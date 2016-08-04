# Original version at http://pythonfiddle.com/nfl-players-list/

import urllib2, re, time, pprint, csv

def get_team_list():
    """
    Returns a list of tuples where each tuple looks like (teamID, teamName)
    """
    # Download the list of teams & return all matches to the regular reTeamData regular expression.

    reTeamData = re.compile('<option value="[0-9]+"')

    team_list_url = 'http://www.nfl.com/players/search?category=team&playerType=current'

    html_data = urllib2.urlopen(team_list_url).read()

    html_tags = re.findall('<option value="[0-9]+">[A-Za-z0-9 ]+</option>', html_data)

    team_ids = map(lambda s: re.search('[0-9]+', s).group(0), html_tags)

    team_names = map(lambda s: s[21:-9], html_tags) # TODO: Make this more robust

    return zip(team_ids, team_names)


# This regular expression extracts a player's ESPN ID and their first and last names.
re_player_data = re.compile('href="/player/[A-Za-z0-9 ]+/[0-9]+/profile">[A-Za-z0-9, ]+')

# This regular expression extracts the link to the "next" page of the team roster.
re_next_page_url = re.compile('href="([^"]+)">next</a>')

def get_team_players(team_id):
    """
    Return a list of players for a team in the form (playerID, playerLastName, playerFirstName)
    """

    # Download the first page of the team roster and store the list of players.

    nfl_url = 'http://www.nfl.com'

    team_url = nfl_url + '/players/search?category=team&filter=%s&playerType=current' % team_id

    team_page_html = urllib2.urlopen(team_url).read()

    players = re_player_data.findall(team_page_html)

    """
    Check for a "next" page.
    If one is found, then download this "next" page & add players on that page to the list.
    Continue checking for more pages and storing until no more pages are found.
    """
    next_url = re_next_page_url.findall(team_page_html)

    while len(next_url) > 0:
        team_page_html = urllib2.urlopen(nfl_url + next_url[0].replace('&amp;','&')).read()
        players.extend(re_player_data.findall(team_page_html))
        next_url = re_next_page_url.findall(team_page_html)

    player_ids = map(lambda s: re.search('[0-9]+/profile', s).group(0)[:-8], players)

    player_full_names = map(lambda s: re.search('profile">[A-Za-z0-9, ]+', s).group(0)[9:], players)

    first_last_names = map(lambda s: s.split(','), player_full_names)

    player_first_names = map(lambda l: l[0].strip(), first_last_names)

    def get_last_name(l):
        try:
            return l[1].strip()
        except:
            return ''

    player_last_names = map(get_last_name, first_last_names)

    return zip(player_ids, player_first_names, player_last_names)

"""
The following regular expressions extract the desired information from the player's profile page.
"""
reHeight = re.compile('<strong>Height</strong>: ([^ \r\n]+)')
reWeight = re.compile('<strong>Weight</strong>: ([^ \r\n]+)')
reAge = re.compile('<strong>Age</strong>: ([^ \r\n]+)')
rePosition = re.compile('<span class="player-number">#[0-9 ]+[A-Za-z0-9 ]+</span>')

# TODO: Add experience
def get_player_info(player_id, f_name, l_name, team_name):
    """
    Returns the player's info.
    """

    nfl_url = 'http://www.nfl.com/player/'

    player_url = nfl_url + f_name + l_name + '/' + str(player_id) + "/profile"

    try:
        pageData = urllib2.urlopen(
            nfl_url + f_name + l_name + '/' + str(player_id) + "/profile"
        ).read()

        heightTokens = reHeight.findall(pageData)[0].split('-')
        height = int(heightTokens[0]) * 12 + int(heightTokens[1])

        position_html = rePosition.findall(pageData)[0]

        position = position_html.split('#')[1][3:-7]

        return {
            'first name': f_name,
            'last name' : l_name,
            'position': position,
            'height': height,
            'weight': int(reWeight.findall(pageData)[0]),
            'age': int(reAge.findall(pageData)[0]),
            'team': team_name,
            'player id' : str(player_id),
            'fetch status' : 'Success'
        }

    except:
        print 'Failed to load', player_id
        return {
            'first name': f_name,
            'last name' : l_name,
            'position': '',
            'height': '',
            'weight': '',
            'age': '',
            'team': team_name,
            'player id' : str(player_id),
            'fetch status' : 'Failure'
        }

def write_csv():

    # Open the CSV file for output.
    csvFile = csv.writer(open('players.csv', 'w'), delimiter=',', quotechar='"')

    # Download the list of teams
    teams = get_team_list()

    # Write Headers
    csvFile.writerow([
        'FirstName',
        'LastName',
        'Position',
        'Height',
        'Weight',
        'Age',
        'Team',
        'PlayerID',
        'FetchStatus'
    ])

    # For each team, download the list of players
    for team in teams:

        print 'Retrieving players from the', team[1]

        players = get_team_players(team[0])

        # For each player, download their info and write it to the CSV file
        for player in players:

            playerInfo = get_player_info(player[0], player[1], player[2], team[1])

            print playerInfo

            if playerInfo:
                #csvFile.writerow(playerInfo.values())
                csvFile.writerow([
                    playerInfo['first name'],
                    playerInfo['last name'],
                    playerInfo['position'],
                    str(playerInfo['height']),
                    str(playerInfo['weight']),
                    str(playerInfo['age']),
                    playerInfo['team'],
                    playerInfo['player id'],
                    playerInfo['fetch status']
                ])

            time.sleep(0.1)
