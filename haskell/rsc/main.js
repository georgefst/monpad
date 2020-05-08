var username = document.currentScript.getAttribute("username");

var wsAddress = document.currentScript.getAttribute("wsAddress");

var ws = new WebSocket(wsAddress);

//TODO waiting on this before even initialising Elm ensures no race condition, but is it a bit slow?
    // also what if the websocket can't be opened? we shoud time out
ws.onopen = function (event) {
    ws.send(username);

    var app = Elm.Main.init({
        flags: username
    });

    app.ports.sendUpdate.subscribe(function (message) {
        ws.send(JSON.stringify(message));
    });
};
