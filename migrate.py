
import json, requests, threading, redis
from datetime import datetime

r = redis.StrictRedis(host='localhost', port=6379, db=1)

# get history from redis

history = r.lrange('history', 0, -1)

# for each match
for matchString in history:
  match = json.loads(matchString)

  p1 = match['p1']
  p2 = match['p2'] 

  key = lambda p: 'games['+p+']'


  # send match to history of each player
  r.lpush(key(p1), matchString)
  r.lpush(key(p2), matchString)


