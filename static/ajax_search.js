
$('.typeahead').typeahead({
    source: function (query, process) {
        return $.get('/search', { query: query }, function (data) {
            return process(data);
        })
    },
    matcher: function(item){
      return true 
    },
    updater: function(item){
      window.location.href = '/player/' + encodeURIComponent(item)
    }
});

