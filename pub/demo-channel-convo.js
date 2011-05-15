// Jquery event handler that will trigger when the page is ready to be
// manipulated.
$(document).ready(function() {
    /// commented out test code used to play around with jquery.
    /*$("a").click(function(event){
        alert("An alert on the ready event!");
        jQuery('#foo').after("<p>hi</p>");
        event.preventDefault();
    });*/
    // $('a').addClass('test');

    // Start our loop so we can continously update the page without user
    // input.
    doTimer();  
});


var time = null;                // Current timeout function.
var timerOnP = null;            // Did we turn the timer on yet?

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
        $.get("/api/beta/channel/log", { id: ($('p').length ) },
              function(data){
                  $('#foo').after(data);
              });
    }, 1000);
};


// Intended to be run in a little text box above the channel message logs
// for the #shanai channel on the PO beta server.
//
// When the user hits enter with text in this box, we send it to the server
// as something that Shanai the PO AI bot is to say.
//
// The server checks that the IP of the sender is valid before passing the
// message on.
function runScript(e) {
    if (e.keyCode == 13) {
        var tb = document.getElementById("txt");
        $.get("/api/beta/send/channel-message/",
              { msg: tb.value },
              function (data){
                  document.getElementById("txt").value = "";
              });
        return false;
    }
};