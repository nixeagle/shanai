// Jquery event handler that will trigger when the page is ready to be
// manipulated.
$(document).ready(function() {
    /// commented out test code used to play around with jquery.
    /*$("a").click(function(event){
        event.preventDefault();
    });*/

    // Start our loop so we can continously update the page without user
    // input.
    doTimer();
    $("#tier").hover(
        function () {$("div.hover").show();},
        function () {$("div.hover").hide();}
    );
});


var time = null;                // Current timeout function.
var timerOnP = null;            // Did we turn the timer on yet?
var jsonreturnval = null;
// Only turn the timer on if we have not already done so.
function doTimer() {
    if (!timerOnP) {
        timerOnP = true;
        return timedCount();
    };
};
function timedCount(count) {
    if (count === undefined) {
        count = 0;
    };

    return time = setTimeout(function () {
        timedCount(count + 1);
        $.get("/api/battles/current", { },
              function(data){
                  jsonreturnval = data;
                  $("#turn span")[0].innerHTML = data.turn;
                  $("#tier")[0].innerHTML = data.tier;
                  $("#opponent")[0].innerHTML = data.challenged.name;
                  for(var i = 0; i < 6; i++) {
                      poke =  data.challenger.pokemon[i];
                      $("#player span")[i].innerHTML = poke.nickname;
                      if (poke.status == 'ko') {
                          $("#player span:nth-child(" + (i + 1) + ")").addClass("koed");
                      } else {
                          $("#player span:nth-child(" + (i + 1) + ")").removeClass("koed");
                      }
                  }
                  for(var i = 0; i < 6; i++) {
                      poke = data.challenged.pokemon[i];
                      if (poke) {
                          $("#trainer span")[i].innerHTML = poke.nickname;
                          if (poke.status == 'ko') {
                              $("#trainer span:nth-child(" + (i + 1) + ")").addClass("koed");
                          } else {
                              $("#trainer span:nth-child(" + (i + 1) + ")").removeClass("koed");
                          }
                      } else {
                          $("#trainer span")[i].innerHTML = "??";
                          $("#trainer span:nth-child(" + (i + 1) + ")").removeClass("koed");
                      }
                  }
                  //$('#foo').after(data);
              });
    }, 1000);
};

function toggleHover(e) {
    $("tier")[0].css("display","block");
}
// Intended to be run in a little text box above the channel message logs
// for the #shanai channel on the PO beta server.
//
// When the user hits enter with text in this box, we send it to the server
// as something that Shanai the PO AI bot is to say.
//
// The server checks that the IP of the sender is valid before passing the
// message on.

