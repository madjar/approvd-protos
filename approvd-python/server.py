import approvd
from flask import Flask, request
app = Flask(__name__)

@app.route('/payload', methods=['POST'])
def root():
    event_type = request.headers.get('X-Github-Event')
    if event_type == 'issue_comment':
        # XXX: need to check this is actually a pull request
        issue_number = request.json['issue']['number']
        app.logger.info('Comment on issue %s', issue_number)
        approvd.handle_pull(issue_number)
    else:
        app.logger.debug("Received %s event:\n%s", event_type, request.json)
    return 'Okay'

if __name__ == '__main__':
    app.run(debug=True)
