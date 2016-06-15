from __future__ import print_function
import requests
import pyquery.pyquery as pq
import pprint
import json
import argparse
import urllib
import os
import subprocess as sp

tabula_jar = os.path.expanduser('~/Downloads/tabula-0.9.0-SNAPSHOT-jar-with-dependencies.jar')

parser = argparse.ArgumentParser()
parser.add_argument('-S', '--subdomain', help='Subdomain of the data site: {subdomain}.alkamelsystems.com', default='fiawec')
parser.add_argument('-s', '--season', help='Season of the race', default='2016')
parser.add_argument('-e', '--event', help='Event of the race')
parser.add_argument('--list-seasons', help='List the seasons available', default=False, action='store_true')
parser.add_argument('--list-events', help='List the events available', default=False, action='store_true')
parser.add_argument('-p', '--path', help='Path to save the files')

args = parser.parse_args()

subdomain = args.subdomain

host = 'http://{}.alkamelsystems.com/'.format(subdomain)

def get_options(season_key=None):
    post = requests.post(host + 'top.php', params={'season': season_key})
    assert post.status_code == 200
    top_text = post.text
    Top = pq.PyQuery(top_text)

    def parse_select(selector):
        return {
            option.text: option.attrib['value']
            for option in Top(selector + ' > option')
        }

    seasons = parse_select('select[name="season"]')
    events = parse_select('select[name="event"]')

    return seasons, events

seasons, events_ = get_options()
season_key = args.season if args.season in seasons.values() else seasons.get(args.season)

assert seasons, "Seasons are empty. season={}, event={}".format(season_key, args.event)
assert season_key is not None, "Season does not exist: {}".format(args.season)

_, events = get_options(season_key=season_key)

if args.list_seasons:
    print(seasons)
    if not args.list_events: exit()

if args.list_events:
    print(events)
    exit()

assert events, "Events are empty. season={}, event={}".format(season_key, args.event)
event_key = event_key if args.event in events.values() else events.get((args.event or '').upper())
assert event_key is not None, "Event does not exist: {}".format(args.event)

params={'season': season_key, 'event': event_key}
get = requests.get(host + 'tree.php', params=params)
assert get.status_code == 200
tree_text = get.text
Tree = pq.PyQuery(tree_text)
subtree = Tree('td.p > div')

def parse_tree_gen(subtree, **opts):
    title = None
    href = None
    it = iter(subtree)
    try:
        while True:
            node = next(it)
            node_id = node.attrib['id']
            assert node_id.startswith('jt') and not node_id.endswith('son')

            link = Tree(node).children('a')[0]
            href = link.attrib.get('href')
            is_csv = href is not None and href.endswith('CSV')
            title = (''.join(node.xpath("text()")).strip() if href is None
                     else ''.join(link.xpath("text()")).strip())

            if href is None:
                node = next(it)
                node_id = node.attrib['id']
                assert node_id.startswith('jt') and node_id.endswith('son')
                assert href is None
                yield title, parse_tree(Tree(node).children(), **opts)
            else:
                file_type = "CSV" if is_csv else "PDF"
                key = ((title, file_type) if opts.get('tuple_key')
                       else "{} ({})".format(title, file_type))
                yield key, href

    except StopIteration:
        pass


def parse_tree(subtree, **opts):
    return {k: v for k, v in parse_tree_gen(subtree, **opts)}


def get_grid_path(series):
    return series["Race"][("Starting Grid", "PDF")]


def get_hour_key(series):
    return "Hour 24" if "Hour 24" in series["Race"] else "Hour 6"


def get_classification_path(series):
    return series["Race"][get_hour_key(series)][("Classification", "CSV")]


def get_analysis_path(series):
    return series["Race"][get_hour_key(series)][("Chronological Analysis", "CSV")]


link_tree = parse_tree(subtree, tuple_key=True)
assert link_tree
assert "FIA WEC" in link_tree, repr(link_tree)

#print(json.dumps(parse_tree(subtree), indent=2))
grid_url = host + get_grid_path(link_tree["FIA WEC"])
classification_url = host + get_classification_path(link_tree["FIA WEC"])
analysis_url = host + get_analysis_path(link_tree["FIA WEC"])

def download_file(url, filename):
    print("Downloading: {} -> {}".format(url, filename))
    return urllib.urlretrieve(url, filename)

prefix = args.path or '{} {}'.format(season_key, event_key)
try:
    os.stat(prefix)
except OSError:
    os.mkdir(prefix)

download_file(grid_url, prefix + '/Grid.pdf')
download_file(classification_url, prefix + '/Classification.csv')
download_file(analysis_url, prefix + '/Analysis.csv')

if tabula_jar:
    sp.call([
        'java', '-client', '-jar', tabula_jar,
        '-i', '--area', '200,240,810,370', '-o', prefix + '/Grid.csv', prefix + '/Grid.pdf'
    ])
