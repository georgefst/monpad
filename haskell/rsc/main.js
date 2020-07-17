// use fullscreen when in landscape
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

const wsPort = document.currentScript.getAttribute("wsPort");
const wsAddress = "ws://" + location.hostname + ":" + wsPort;

const ws = new WebSocket(wsAddress);

ws.onopen = function (event) {
    const app = Elm.Main.init({
        flags: elmFlags
    });
    app.ports.sendUpdatePort.subscribe(function (message) {
        ws.send(JSON.stringify(message));
    });
};
