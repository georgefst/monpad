const attr = s => document.currentScript.attributes.getNamedItem(s).value

const username = new URLSearchParams(window.location.search).get("username")

const wsAddress = "ws://" + location.hostname + ":" + attr("wsPort") + "?" + new URLSearchParams({ username })
const ws = new WebSocket(wsAddress)

const layouts = JSON.parse(attr("layouts"))

window.onbeforeunload = () => ws.close()

ws.onopen = _event => {
    // Elm
    elmInitialised = false
    const app = Elm.Main.init({
        flags: { username, layouts }
    })
    //TODO this (waiting for an initialisation message)
    // is all just a workaround for the fact that Elm has no built-in way of signalling that 'init' has finished
    app.ports.initPort.subscribe(() => {
        elmInitialised = true
        dispatchEvent(new Event("elmInit"))
    })

    // fullscreen
    app.ports.fullscreenPort.subscribe(() => {
        document.documentElement.requestFullscreen()
    })
    document.onfullscreenchange = _event => {
        app.ports.fullscreenChangePort.send(
            !(document.fullscreenElement == null)
        )
    }

    // update messages
    app.ports.sendUpdatePort.subscribe(message => ws.send(JSON.stringify(message)))
    ws.addEventListener("message", event => {
        const send = () => app.ports.receiveUpdatePort.send(JSON.parse(event.data))
        if (elmInitialised) {
            send()
        } else {
            addEventListener("elmInit", () => {
                setTimeout(send, 0)
            })
        }
    })

    // audio
    app.ports.audioPort.subscribe(url => {
        audio = new Audio(url)
        audio.play()
    })

    // vibration
    app.ports.vibratePort.subscribe(intervals => {
        window.navigator.vibrate(intervals)
    })

    // logging
    app.ports.logPort.subscribe(console.log)
}
