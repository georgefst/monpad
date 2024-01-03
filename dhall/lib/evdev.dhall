let AbsAxis =
      < AbsX
      | AbsY
      | AbsZ
      | AbsRx
      | AbsRy
      | AbsRz
      | AbsThrottle
      | AbsRudder
      | AbsWheel
      | AbsGas
      | AbsBrake
      | AbsHat0x
      | AbsHat0y
      | AbsHat1x
      | AbsHat1y
      | AbsHat2x
      | AbsHat2y
      | AbsHat3x
      | AbsHat3y
      | AbsPressure
      | AbsDistance
      | AbsTiltX
      | AbsTiltY
      | AbsToolWidth
      | AbsVolume
      | AbsMisc
      | AbsReserved
      | AbsMtSlot
      | AbsMtTouchMajor
      | AbsMtTouchMinor
      | AbsMtWidthMajor
      | AbsMtWidthMinor
      | AbsMtOrientation
      | AbsMtPositionX
      | AbsMtPositionY
      | AbsMtToolType
      | AbsMtBlobId
      | AbsMtTrackingId
      | AbsMtPressure
      | AbsMtDistance
      | AbsMtToolX
      | AbsMtToolY
      >

let RelAxis =
      < RelX
      | RelY
      | RelZ
      | RelRx
      | RelRy
      | RelRz
      | RelHwheel
      | RelDial
      | RelWheel
      | RelMisc
      | RelHWheelHiRes
      | RelWheelHiRes
      | RelReserved
      >

let Axis = < Abs : AbsAxis | Rel : RelAxis >

let AxisInfo = { axis : Axis, multiplier : Integer }

let Key =
      < KeyReserved
      | KeyEsc
      | Key1
      | Key2
      | Key3
      | Key4
      | Key5
      | Key6
      | Key7
      | Key8
      | Key9
      | Key0
      | KeyMinus
      | KeyEqual
      | KeyBackspace
      | KeyTab
      | KeyQ
      | KeyW
      | KeyE
      | KeyR
      | KeyT
      | KeyY
      | KeyU
      | KeyI
      | KeyO
      | KeyP
      | KeyLeftbrace
      | KeyRightbrace
      | KeyEnter
      | KeyLeftctrl
      | KeyA
      | KeyS
      | KeyD
      | KeyF
      | KeyG
      | KeyH
      | KeyJ
      | KeyK
      | KeyL
      | KeySemicolon
      | KeyApostrophe
      | KeyGrave
      | KeyLeftshift
      | KeyBackslash
      | KeyZ
      | KeyX
      | KeyC
      | KeyV
      | KeyB
      | KeyN
      | KeyM
      | KeyComma
      | KeyDot
      | KeySlash
      | KeyRightshift
      | KeyKpasterisk
      | KeyLeftalt
      | KeySpace
      | KeyCapslock
      | KeyF1
      | KeyF2
      | KeyF3
      | KeyF4
      | KeyF5
      | KeyF6
      | KeyF7
      | KeyF8
      | KeyF9
      | KeyF10
      | KeyNumlock
      | KeyScrolllock
      | KeyKp7
      | KeyKp8
      | KeyKp9
      | KeyKpminus
      | KeyKp4
      | KeyKp5
      | KeyKp6
      | KeyKpplus
      | KeyKp1
      | KeyKp2
      | KeyKp3
      | KeyKp0
      | KeyKpdot
      | KeyZenkakuhankaku
      | Key102nd
      | KeyF11
      | KeyF12
      | KeyRo
      | KeyKatakana
      | KeyHiragana
      | KeyHenkan
      | KeyKatakanahiragana
      | KeyMuhenkan
      | KeyKpjpcomma
      | KeyKpenter
      | KeyRightctrl
      | KeyKpslash
      | KeySysrq
      | KeyRightalt
      | KeyLinefeed
      | KeyHome
      | KeyUp
      | KeyPageup
      | KeyLeft
      | KeyRight
      | KeyEnd
      | KeyDown
      | KeyPagedown
      | KeyInsert
      | KeyDelete
      | KeyMacro
      | KeyMute
      | KeyVolumedown
      | KeyVolumeup
      | KeyPower
      | KeyKpequal
      | KeyKpplusminus
      | KeyPause
      | KeyScale
      | KeyKpcomma
      | KeyHangeul
      | KeyHanja
      | KeyYen
      | KeyLeftmeta
      | KeyRightmeta
      | KeyCompose
      | KeyStop
      | KeyAgain
      | KeyProps
      | KeyUndo
      | KeyFront
      | KeyCopy
      | KeyOpen
      | KeyPaste
      | KeyFind
      | KeyCut
      | KeyHelp
      | KeyMenu
      | KeyCalc
      | KeySetup
      | KeySleep
      | KeyWakeup
      | KeyFile
      | KeySendfile
      | KeyDeletefile
      | KeyXfer
      | KeyProg1
      | KeyProg2
      | KeyWww
      | KeyMsdos
      | KeyScreenlock
      | KeyRotateDisplay
      | KeyCyclewindows
      | KeyMail
      | KeyBookmarks
      | KeyComputer
      | KeyBack
      | KeyForward
      | KeyClosecd
      | KeyEjectcd
      | KeyEjectclosecd
      | KeyNextsong
      | KeyPlaypause
      | KeyPrevioussong
      | KeyStopcd
      | KeyRecord
      | KeyRewind
      | KeyPhone
      | KeyIso
      | KeyConfig
      | KeyHomepage
      | KeyRefresh
      | KeyExit
      | KeyMove
      | KeyEdit
      | KeyScrollup
      | KeyScrolldown
      | KeyKpleftparen
      | KeyKprightparen
      | KeyNew
      | KeyRedo
      | KeyF13
      | KeyF14
      | KeyF15
      | KeyF16
      | KeyF17
      | KeyF18
      | KeyF19
      | KeyF20
      | KeyF21
      | KeyF22
      | KeyF23
      | KeyF24
      | KeyPlaycd
      | KeyPausecd
      | KeyProg3
      | KeyProg4
      | KeyDashboard
      | KeySuspend
      | KeyClose
      | KeyPlay
      | KeyFastforward
      | KeyBassboost
      | KeyPrint
      | KeyHp
      | KeyCamera
      | KeySound
      | KeyQuestion
      | KeyEmail
      | KeyChat
      | KeySearch
      | KeyConnect
      | KeyFinance
      | KeySport
      | KeyShop
      | KeyAlterase
      | KeyCancel
      | KeyBrightnessdown
      | KeyBrightnessup
      | KeyMedia
      | KeySwitchvideomode
      | KeyKbdillumtoggle
      | KeyKbdillumdown
      | KeyKbdillumup
      | KeySend
      | KeyReply
      | KeyForwardmail
      | KeySave
      | KeyDocuments
      | KeyBattery
      | KeyBluetooth
      | KeyWlan
      | KeyUwb
      | KeyUnknown
      | KeyVideoNext
      | KeyVideoPrev
      | KeyBrightnessCycle
      | KeyBrightnessAuto
      | KeyDisplayOff
      | KeyWwan
      | KeyRfkill
      | KeyMicmute
      | Btn0
      | Btn1
      | Btn2
      | Btn3
      | Btn4
      | Btn5
      | Btn6
      | Btn7
      | Btn8
      | Btn9
      | BtnLeft
      | BtnRight
      | BtnMiddle
      | BtnSide
      | BtnExtra
      | BtnForward
      | BtnBack
      | BtnTask
      | BtnJoystick
      | BtnThumb
      | BtnThumb2
      | BtnTop
      | BtnTop2
      | BtnPinkie
      | BtnBase
      | BtnBase2
      | BtnBase3
      | BtnBase4
      | BtnBase5
      | BtnBase6
      | BtnDead
      | BtnA
      | BtnB
      | BtnC
      | BtnX
      | BtnY
      | BtnZ
      | BtnTl
      | BtnTr
      | BtnTl2
      | BtnTr2
      | BtnSelect
      | BtnStart
      | BtnMode
      | BtnThumbl
      | BtnThumbr
      | BtnToolPen
      | BtnToolRubber
      | BtnToolBrush
      | BtnToolPencil
      | BtnToolAirbrush
      | BtnToolFinger
      | BtnToolMouse
      | BtnToolLens
      | BtnToolQuinttap
      | BtnTouch
      | BtnStylus
      | BtnStylus2
      | BtnToolDoubletap
      | BtnToolTripletap
      | BtnToolQuadtap
      | BtnGearDown
      | BtnGearUp
      | KeyOk
      | KeySelect
      | KeyGoto
      | KeyClear
      | KeyPower2
      | KeyOption
      | KeyInfo
      | KeyTime
      | KeyVendor
      | KeyArchive
      | KeyProgram
      | KeyChannel
      | KeyFavorites
      | KeyEpg
      | KeyPvr
      | KeyMhp
      | KeyLanguage
      | KeyTitle
      | KeySubtitle
      | KeyAngle
      | KeyZoom
      | KeyMode
      | KeyKeyboard
      | KeyScreen
      | KeyPc
      | KeyTv
      | KeyTv2
      | KeyVcr
      | KeyVcr2
      | KeySat
      | KeySat2
      | KeyCd
      | KeyTape
      | KeyRadio
      | KeyTuner
      | KeyPlayer
      | KeyText
      | KeyDvd
      | KeyAux
      | KeyMp3
      | KeyAudio
      | KeyVideo
      | KeyDirectory
      | KeyList
      | KeyMemo
      | KeyCalendar
      | KeyRed
      | KeyGreen
      | KeyYellow
      | KeyBlue
      | KeyChannelup
      | KeyChanneldown
      | KeyFirst
      | KeyLast
      | KeyAb
      | KeyNext
      | KeyRestart
      | KeySlow
      | KeyShuffle
      | KeyBreak
      | KeyPrevious
      | KeyDigits
      | KeyTeen
      | KeyTwen
      | KeyVideophone
      | KeyGames
      | KeyZoomin
      | KeyZoomout
      | KeyZoomreset
      | KeyWordprocessor
      | KeyEditor
      | KeySpreadsheet
      | KeyGraphicseditor
      | KeyPresentation
      | KeyDatabase
      | KeyNews
      | KeyVoicemail
      | KeyAddressbook
      | KeyMessenger
      | KeyDisplaytoggle
      | KeySpellcheck
      | KeyLogoff
      | KeyDollar
      | KeyEuro
      | KeyFrameback
      | KeyFrameforward
      | KeyContextMenu
      | KeyMediaRepeat
      | Key10channelsup
      | Key10channelsdown
      | KeyImages
      | KeyDelEol
      | KeyDelEos
      | KeyInsLine
      | KeyDelLine
      | KeyFn
      | KeyFnEsc
      | KeyFnF1
      | KeyFnF2
      | KeyFnF3
      | KeyFnF4
      | KeyFnF5
      | KeyFnF6
      | KeyFnF7
      | KeyFnF8
      | KeyFnF9
      | KeyFnF10
      | KeyFnF11
      | KeyFnF12
      | KeyFn1
      | KeyFn2
      | KeyFnD
      | KeyFnE
      | KeyFnF
      | KeyFnS
      | KeyFnB
      | KeyBrlDot1
      | KeyBrlDot2
      | KeyBrlDot3
      | KeyBrlDot4
      | KeyBrlDot5
      | KeyBrlDot6
      | KeyBrlDot7
      | KeyBrlDot8
      | KeyBrlDot9
      | KeyBrlDot10
      | KeyNumeric0
      | KeyNumeric1
      | KeyNumeric2
      | KeyNumeric3
      | KeyNumeric4
      | KeyNumeric5
      | KeyNumeric6
      | KeyNumeric7
      | KeyNumeric8
      | KeyNumeric9
      | KeyNumericStar
      | KeyNumericPound
      | KeyNumericA
      | KeyNumericB
      | KeyNumericC
      | KeyNumericD
      | KeyCameraFocus
      | KeyWpsButton
      | KeyTouchpadToggle
      | KeyTouchpadOn
      | KeyTouchpadOff
      | KeyCameraZoomin
      | KeyCameraZoomout
      | KeyCameraUp
      | KeyCameraDown
      | KeyCameraLeft
      | KeyCameraRight
      | KeyAttendantOn
      | KeyAttendantOff
      | KeyAttendantToggle
      | KeyLightsToggle
      | BtnDpadUp
      | BtnDpadDown
      | BtnDpadLeft
      | BtnDpadRight
      | KeyAlsToggle
      | KeyButtonconfig
      | KeyTaskmanager
      | KeyJournal
      | KeyControlpanel
      | KeyAppselect
      | KeyScreensaver
      | KeyVoicecommand
      | KeyBrightnessMin
      | KeyBrightnessMax
      | KeyKbdinputassistPrev
      | KeyKbdinputassistNext
      | KeyKbdinputassistPrevgroup
      | KeyKbdinputassistNextgroup
      | KeyKbdinputassistAccept
      | KeyKbdinputassistCancel
      | BtnTriggerHappy1
      | BtnTriggerHappy2
      | BtnTriggerHappy3
      | BtnTriggerHappy4
      | BtnTriggerHappy5
      | BtnTriggerHappy6
      | BtnTriggerHappy7
      | BtnTriggerHappy8
      | BtnTriggerHappy9
      | BtnTriggerHappy10
      | BtnTriggerHappy11
      | BtnTriggerHappy12
      | BtnTriggerHappy13
      | BtnTriggerHappy14
      | BtnTriggerHappy15
      | BtnTriggerHappy16
      | BtnTriggerHappy17
      | BtnTriggerHappy18
      | BtnTriggerHappy19
      | BtnTriggerHappy20
      | BtnTriggerHappy21
      | BtnTriggerHappy22
      | BtnTriggerHappy23
      | BtnTriggerHappy24
      | BtnTriggerHappy25
      | BtnTriggerHappy26
      | BtnTriggerHappy27
      | BtnTriggerHappy28
      | BtnTriggerHappy29
      | BtnTriggerHappy30
      | BtnTriggerHappy31
      | BtnTriggerHappy32
      | BtnTriggerHappy33
      | BtnTriggerHappy34
      | BtnTriggerHappy35
      | BtnTriggerHappy36
      | BtnTriggerHappy37
      | BtnTriggerHappy38
      | BtnTriggerHappy39
      | BtnTriggerHappy40
      >

in  { AbsAxis, RelAxis, Axis, AxisInfo, Key }
