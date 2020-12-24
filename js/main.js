// use fullscreen when in landscape
window.screen.orientation.onchange = () => {
    console.log(this.type)
    switch (this.type) {
        case "landscape-primary":
        case "landscape-secondary":
            document.documentElement.requestFullscreen()
            break
        default:
            document.exitFullscreen()
            break
    }
}

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
    app.ports.sendUpdatePort.subscribe(message => ws.send(JSON.stringify(message)))
}
