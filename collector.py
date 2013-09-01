import json, requests, threading, redis
from datetime import datetime


from socketIO_client import SocketIO

import logging

logging.basicConfig(format='%(levelname)s %(asctime)s %(message)s', datefmt='%d/%m/%Y %I:%M:%S %p', level=logging.INFO)

r = redis.StrictRedis(host='localhost', port=6379, db=1)

socket = SocketIO('www-cdn-twitch.saltybet.com', 8000)


#
# Improvements to naive elo rating system:
#   * vary k as inversely proportional to the number of games played, so well estab#     lished players are insulated from upsets
#   * account for locality effects. certain players only playing each other
#     get ratings that are not useful for comparing against others outside
#     the cohort.
#

class Player:
  def __init__(self, name):
    self.name = name
    if self.rating() == None:
      r.zadd("players", 10.0, name)
  def rating(self):
    return r.zscore("players", self.name)
  def addTo(self, x):
    new = r.zincrby("players", self.name, x)
    old = new - x
    print 'score of', self.name, 'went from', old, 'to', new
    return new
  def q(self):
    return 10**(self.rating()/10.0)
  def expected(a, b):
    aq, bq = a.q(), b.q()
    return aq/(aq + bq)
  def beat(a, b):
    print a.name, 'Wins!'
    k = 3.0
    expected = a.expected(b)
    d = k * (1.0 - expected) 
    a.addTo(d)
    b.addTo(-d)
    return expected
  def __eq__(a, b):
    return a.name == b.name


def end_game(p1, p2, p1Won):
  if p1Won:
    odds = p1.beat(p2)
  else:
    odds = p2.beat(p1)
   
  matchOutcome = json.dumps({
    'p1':p1.name,
    'p2':p2.name,
    'p1Won':p1Won,
    'odds' : odds})

  # add match outcome to list of matches
  r.lpush('history', matchOutcome)

  # add match to match history of each player
  r.lpush('games['+p1.name+']', matchOutcome)
  r.lpush('games['+p2.name+']', matchOutcome)

  # publish the match outcome
  r.publish('game_over', matchOutcome)

def start_game(p1, p2):
  # print info about the current game locally.
  odds = p1.expected(p2)
  logging.info('{} vs. {}, odds: {}'.format(p1.name, p2.name, round(odds * 1000.0)/10.0))

  # send information about the current match to redis.
  # should probably just put this info intothe history list, and update
  # the outcome when we get it.
  r.set('p1', p1.name)
  r.set('p2', p2.name)
  r.set('odds', odds)

  newMatch = json.dumps({
      'p1': p1.name
    , 'p2': p2.name
    , 'odds': odds
  })

  r.publish('start_game', newMatch)


lastStatus = ''

def onMessage(msg):
  global lastStatus


  try:


    response = requests.get('http://www.saltybet.com/state.json') 
 
    data = response.json()
  
    status = data['status']
    changed = lastStatus != status
  
    p1 = Player(data['p1name'])
    p2 = Player(data['p2name'])
  
    if changed:
      logging.info('status: {}'.format(status))
      if status == 'open':
        if not (lastStatus == '1' or lastStatus == '2'):
          logging.warning('missed status')
        start_game(p1, p2)
      elif status == 'locked':
        if lastStatus != 'open':
          logging.warning('missed status')
      elif status == '1':
        end_game(p1, p2, True)
      elif status == '2':
        end_game(p1, p2, False)
      else:
        logging.warning('unhandled status: {}'.format(status))

    lastStatus = status
  except Exception as e:
    print e

socket.on('message', onMessage)

while 1:
  #socket.emit('message')
  socket.wait(seconds=1)

