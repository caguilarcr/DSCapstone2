import requests
import csv
from bs4 import BeautifulSoup

URL = 'https://www.nba.com/history/awards/all-nba-team'
page = requests.get(URL)
soup = BeautifulSoup(page.content, 'html.parser')
wrapper = soup.find(class_='field-item').find(class_='field-item')

def get_players(nodo):
    space = u'\xa0'
    players = str(nodo).replace(
        '<p>', '').replace('</p>', '').replace(
        '<strong>', '').replace('</strong>', '').replace(
        space, u'').replace(space, '').replace(
        space, '').replace(space, '').replace(
        space, '').replace('\n','').split('<br/>')
    players_name = []
    for player in players:
        name = player.split(', ')[0].split(':')
        players_name.append(name[len(name) - 1])
    return players_name

temporada = wrapper.find('h3')
with open('Data/all_nba_teams.csv', 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(['Year', 'Player'])
    while temporada:
        anio = int(str(temporada).split('-')[0].replace('<h3><strong>', '')) + 1
        first_team = temporada.find_next_sibling('p').find_next_sibling('p')
        second_team = first_team.find_next_sibling('p').find_next_sibling('p')
        players = get_players(first_team) + get_players(second_team)
        for player in players:
            writer.writerow([anio, player])
        temporada = temporada.find_next_sibling('h3')







