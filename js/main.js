const attr = s => document.currentScript.attributes.getNamedItem(s).value

const username = attr("username")

const wsAddress = "ws://" + location.hostname + ":" + attr("wsPort") + "?" + new URLSearchParams({ username })
const ws = new WebSocket(wsAddress)

const layouts = JSON.parse(attr("layouts"))

window.onbeforeunload = () => ws.close()

ws.onopen = event => {
    elmInitialised = false
    const app = Elm.Main.init({
        flags: { username, layouts }
    })
    //TODO this (waiting for an initialisation message)
    // is all just a workaround for the fact that Elm has no built-in way of signalling that 'init' has finished
    app.ports.initPort.subscribe(message => {
        elmInitialised = true
        dispatchEvent(new Event("elmInit"))
    })
    app.ports.fullscreenPort.subscribe(message => {
        document.documentElement.requestFullscreen()
    })
    document.onfullscreenchange = event => {
        app.ports.fullscreenChangePort.send(
            !(document.fullscreenElement == null)
        )
    }
    app.ports.sendUpdatePort.subscribe(message => ws.send(JSON.stringify(message)))
    ws.addEventListener("message", event => {
        const send = () => app.ports.receiveUpdatePort.send(JSON.parse(event.data))
        if (elmInitialised) {
            send()
        } else {
            addEventListener("elmInit", event => {
                setTimeout(send, 0)
            })
        }
    })
    app.ports.audioPort.subscribe(url => {
        audio = new Audio(url)
        audio.play()
    })
    app.ports.logPort.subscribe(message => console.log(message))
}
