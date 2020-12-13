// use fullscreen when in landscape
window.screen.orientation.onchange = function () {
    console.log(this.type)
    switch (this.type) {
        case "landscape-primary":
        case "landscape-secondary":
            document.documentElement.requestFullscreen();
            break;
        default:
            document.exitFullscreen();
            break;
    };
}

const username = document.currentScript.getAttribute("username");

const wsPort = document.currentScript.getAttribute("wsPort");
const wsAddress = "ws://" + location.hostname + ":" + wsPort + "/" + username;
const ws = new WebSocket(wsAddress);

const layout = JSON.parse(document.currentScript.getAttribute("layout"));

window.onbeforeunload = function () {
    ws.close()
}

ws.onopen = function (event) {
    const app = Elm.Main.init({
        flags: { username, layout }
    });
    app.ports.sendUpdatePort.subscribe(function (message) {
        ws.send(JSON.stringify(message));
    });
};
