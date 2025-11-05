const attr = s => document.currentScript.attributes.getNamedItem(s).value

const params = new URLSearchParams(window.location.search)
const username = params.get("username")

const wsAddress = "ws://" + location.hostname + ":" + attr("wsPort") + "?" + params
const ws = new WebSocket(wsAddress)
const wsCloseMessage = attr("wsCloseMessage") // we can't do this lazily - `document.currentScript` may not exist later
ws.onclose = event => {
    console.log("WebSocket closed", event)
    alert(wsCloseMessage)
}

const layouts = JSON.parse(attr("layouts"))
const encoding = JSON.parse(attr("encoding"))
const windowTitle = attr("windowTitle");

ws.onopen = _event => {
    // Elm
    elmInitialised = false
    const app = Elm.Main.init({
        flags: {
            username, layouts, encoding, windowTitle,
            supportsFullscreen: document.documentElement.requestFullscreen != null
        }
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
    app.ports.sendUpdatePortJSON.subscribe(message => ws.send(JSON.stringify(message)))
    app.ports.sendUpdatePortBinary.subscribe(message => ws.send(Uint8Array.from(message)))
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

    // reset form
    app.ports.resetFormPort.subscribe(id => document.getElementById(id).reset())

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
