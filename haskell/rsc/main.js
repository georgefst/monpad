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
const wsAddress = "ws://" + location.hostname + ":" + wsPort;

const ws = new WebSocket(wsAddress);

window.onbeforeunload = function () {
    ws.close()
}

ws.onopen = function (event) {
    ws.send(username);
    ws.onmessage = function (message) {
        const layout = JSON.parse(message.data);
        const app = Elm.Main.init({
            flags: { username, layout }
        });
        app.ports.sendUpdatePort.subscribe(function (message) {
            ws.send(JSON.stringify(message));
        });
    }
};
