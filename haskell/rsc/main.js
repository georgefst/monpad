// TODO Firefox doesn't yet give permission to trigger fullscreen from an orientation change
// https://github.com/whatwg/fullscreen/issues/34
// though Firefox actually works kind of alright without fullscreen anyway (Chrome really doesn't)
window.screen.orientation.onchange = function () {
    console.log(this.type)
    switch (this.type) {
        case "landscape-primary":
        case "landscape-secondary":
            document.documentElement.requestFullscreen();
            break;
        default:
            //TODO this errors if we're not fullscreen to begin with, but does that actually matter?
            document.exitFullscreen();
            break;
    };
}

const elmFlags = JSON.parse(document.currentScript.getAttribute("elmFlags"));

const wsAddress = document.currentScript.getAttribute("wsAddress");

const ws = new WebSocket(wsAddress);

//TODO waiting on this before even initialising Elm ensures no race condition, but is it a bit slow?
// also what if the websocket can't be opened? we should time out
ws.onopen = function (event) {
    ws.send(elmFlags.username);

    const app = Elm.Main.init({
        flags: elmFlags
    });

    app.ports.sendUpdate.subscribe(function (message) {
        ws.send(JSON.stringify(message));
    });
};
