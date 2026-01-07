const attrs = document.currentScript.attributes
// TODO not supported - https://github.com/erikd/language-javascript/issues/71
// const optsFilePath = attrs.getNamedItem("optsFile")?.value
const optsFilePath = attrs.getNamedItem("optsFile") != undefined ? attrs.getNamedItem("optsFile").value : undefined
const optsFilePromise = optsFilePath != undefined
    ? fetch(optsFilePath).then(r => r.json()).catch(_ => ({}))
    : Promise.resolve({})
optsFilePromise.then(fileOpts => {

// TODO not supported - https://github.com/erikd/language-javascript/issues/71
// const attr = s => fileOpts[s] ?? attrs.getNamedItem(s)?.value
const attr = s => fileOpts[s] != undefined ? fileOpts[s] : attrs.getNamedItem(s) ? attrs.getNamedItem(s).value : undefined

const params = new URLSearchParams(window.location.search)
const username = params.get("username")

const port = attr("port")
const noWs = !port
const wsAddress = port ? "ws://" + location.hostname + ":" + port + "?" + params : undefined
const ws = wsAddress ? new WebSocket(wsAddress) : undefined
if (!noWs) {
    ws.onclose = event => {
        console.log("WebSocket closed", event)
        alert(attr("ws-close-message"))
    }
}

const layouts = JSON.parse(attr("layouts"))
const encoding = JSON.parse(attr("encoding"))
const windowTitle = attr("window-title");

const runApp = () => {
    // Elm
    elmInitialised = false
    const app = Elm.Main.init({
        node: document.getElementById('elm-root'),
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
    const sendMessage = message => {
        if (noWs) window.parent.document.dispatchEvent(new CustomEvent('monpad-client-update', {detail: message}))
        else ws.send(message)
    }
    app.ports.sendUpdatePortJSON.subscribe(message => sendMessage(JSON.stringify(message)))
    app.ports.sendUpdatePortBinary.subscribe(message => sendMessage(Uint8Array.from(message)))
    const receiveMessage = (message) => {
        const send = () => app.ports.receiveUpdatePort.send(message)
        if (elmInitialised) {
            send()
        } else {
            addEventListener("elmInit", () => {
                setTimeout(send, 0)
            })
        }
    }
    if (noWs) window.parent.document.addEventListener("monpad-server-update", e => receiveMessage(e.detail))
    else ws.addEventListener("message", event => receiveMessage(JSON.parse(event.data)))

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

if (noWs) {
    runApp()
} else {
    ws.onopen = _event => runApp()
}

})
