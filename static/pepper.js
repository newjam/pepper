var stream = new EventSource('/stream')

stream.onerror = function(e) {
  // try again?
  stream = new EventSource('/stream')
  console.log('EventSource error:' + e)
}


stream.addEventListener("game_over", function(e){
  $('table#recentMatches').prepend(e.data)
})

stream.addEventListener("start_game", function(e){
  $('div#currentMatch').html(e.data)
})

