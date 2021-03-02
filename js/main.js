const attr = s => document.currentScript.attributes.getNamedItem(s).value

const username = attr("username")

const wsAddress = "ws://" + location.hostname + ":" + attr("wsPort") + "?" + new URLSearchParams({ username })
const ws = new WebSocket(wsAddress)

const layout = JSON.parse(attr("layout"))

window.onbeforeunload = () => ws.close()

ws.onopen = event => {
    const app = Elm.Main.init({
        flags: { username, layout }
    })
    app.ports.fullscreenPort.subscribe(message => {
        if (document.fullscreenElement == null) {
            document.documentElement.requestFullscreen()
        } else {
            document.exitFullscreen()
        }
    })
    app.ports.sendUpdatePort.subscribe(message => ws.send(JSON.stringify(message)))
    ws.addEventListener("message", event =>
        app.ports.receiveUpdatePort.send(JSON.parse(event.data))
    )
    app.ports.logPort.subscribe(message => console.log(message))
}
