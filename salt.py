import json, requests, threading, redis
from datetime import datetime

r = redis.StrictRedis(host='localhost', port=6379, db=1)

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

def start_game(p1, p2):
  # print info about the current game locally.
  print p1.name, "vs.", p2.name
  odds = p1.expected(p2)
  print round(odds * 1000.0)/10.0, "%"

  # send information about the current match to redis.
  # should probably just put this info intothe history list, and update
  # the outcome when we get it.
  r.set("p1", p1.name)
  r.set("p2", p2.name)
  r.set("odds", odds)

def do_stuff(lastModified, oldData):
  try:
    #global lastModified
    headers = {"If-Modified-Since": lastModified}
    response = requests.get(url="http://saltybet.com/betdata.json", headers=headers)
  
    if response.status_code == 200:
      print "200"
      lastModified = response.headers['last-modified']
      data = response.json()
  
      status = data['status']
  
      p1 = Player(data['p1name'])
      p2 = Player(data['p2name'])
  
      if status == 'open':
        start_game(p1, p2)
      elif status == 'locked':
        print "betting closed"
        pass
      elif status == '1':
        end_game(p1, p2, True)
      elif status == '2':
        end_game(p1, p2, False)
     
      #alert: ""
      #p1name: "Cloud"
      #p1total: "720891"
      #p2name: "Squall"
      #p2total: "313909"
      #status: "1"
  
    elif response.status_code == 304:
      data = oldData
      pass
    else:
      print "unhandled status code"

  except Exception as e:
    print "ERROR!"
    print e
    data = oldData
  # basically, even if there is an exception, repeat in 3 seconds.
  finally:
    threading.Timer(3.0, do_stuff, [lastModified, data]).start()

do_stuff(None, {})

