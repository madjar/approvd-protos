import re
import json
from functools import partial
from pprint import pprint
import requests

CONTEXT = "approvd-python-proto"

MESSAGES = {
    'success': 'The pull request is approved!',
    'pending': 'Waiting for approval',
    'failed': 'The pull request was rejected',
}

with open('token') as f:
    token = f.read().strip()

def request(f, *args, **kwargs):
    base = "https://api.github.com/repos/madjar/approvd-protos/"
    url = base + '/'.join(map(str, args)) + '?access_token=' + token
    return f(url, **kwargs)

get = partial(request, requests.get)
post = partial(request, requests.post)


def contains_approval(comments):
    for comment in comments:
        if re.search(r'^[Aa]pprove?d', comment['body']):
            return True
    return False


def handle_pull(i):
    pull = get('pulls', i).json()
    comments = get('issues', i, "comments").json()
    sha = pull['head']['sha']

    current_state = next((s['state']
                          for s in get("status", sha).json()['statuses']
                          if s['context'] == CONTEXT),
                         None)

    new_state = "success" if contains_approval(comments) else "pending"

    if current_state != new_state:
        data = dict(state=new_state,
                    description=MESSAGES[new_state],
                    context=CONTEXT)
        post("statuses", sha, data=json.dumps(data)).json()
        print('Change the state of {} ({}) to {}'.format(
            i, pull['title'], new_state))

def handle_repo():
    pulls = get('pulls').json()
    for p in pulls:
        if p['state'] == 'open':
            handle_pull(p['number'])

if __name__ == '__main__':
    handle_repo()
