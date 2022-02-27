import { Elm } from "./SongMakerSF.elm"

const app = Elm.SongMakerSF.init()

const Player = MakePlayer()

window.Player ??= Player

// app.ports.start.subscribe(Player.start)
// app.ports.stop.subscribe(Player.stop)
app.ports.updateSteps.subscribe(Player.updateSteps)
// app.ports.playSingleNote.subscribe(Player.playSingleNote)
app.ports.playNote2.subscribe(Player.playNote2)

function notifyColumnChanged(i) {
  return app.ports.selectColumn.send(i)
}

function MakePlayer() {
  const audioContext = newAudioContext()

  const fontPlayer = new WebAudioFontPlayer()
  const presetMap = {
    piano: "_tone_0000_SBLive_sf2", // bassDrum2: "_drum_35_0_Chaos_sf2_file",
    bassDrum1: "_drum_36_0_Chaos_sf2_file",
    snareDrum2: "_drum_40_0_Chaos_sf2_file",
    strings: "_tone_0450_Chaos_sf2_file",
  }
  const presetNames = Object.values(presetMap)
  loadPresets(audioContext, fontPlayer, presetNames)

  const bpm = 120
  const barLengthInSeconds = (4 * 60) / bpm
  const totalBars = 4
  const loopLengthInSeconds = totalBars * barLengthInSeconds
  const noteGap = (1 / 8) * barLengthInSeconds
  const noteDuration = noteGap

  const ticker = new WebAudioFontTicker()

  let steps = null

  function playNote([presetName, note], at = 0) {
    const preset = window[presetMap[presetName]]
    fontPlayer.queueWaveTable(
      audioContext,
      audioContext.destination,
      preset,
      at,
      NoteParser.midi(note),
      noteDuration,
      0.5,
      [],
    )
  }

  function onTick(wallClock, from, to) {
    for (let i = 0; i < steps.length; i++) {
      const noteStartTimeInLoop = i * noteGap
      if (noteStartTimeInLoop >= from && noteStartTimeInLoop < to) {
        const scheduleDelay = noteStartTimeInLoop - from
        setTimeout(() => notifyColumnChanged(i), scheduleDelay - 0.1)
        const scheduleTime = wallClock + scheduleDelay
        steps[i].forEach((data) => playNote(data, scheduleTime))
      }
    }
  }

  return {
    updateSteps(steps_) {
      steps = steps_
    },
    start(steps_) {
      ticker.cancel()
      steps = steps_
      ticker.startTicks(
        audioContext,
        onTick,
        0,
        ticker.lastPosition,
        loopLengthInSeconds,
        () => fontPlayer.cancelQueue(audioContext),
      )
    },
    stop() {
      ticker.cancel()
    },
    playSingleNote(data) {
      playNote(data)
    },
    playNote2({ presetName, startOffset, pitch, duration }) {
      const preset = window[presetMap[presetName]]
      fontPlayer.queueWaveTable(
        audioContext,
        audioContext.destination,
        preset,
        audioContext.currentTime + startOffset / 1000,
        NoteParser.midi(pitch),
        duration / 1000,
        0.5,
        [],
      )
    },
  }
}

function newAudioContext() {
  const AudioContextFunc = window.AudioContext || window["webkitAudioContext"]
  return new AudioContextFunc()
}

function loadPresets(audioContext, fontPlayer, presetNames) {
  presetNames.forEach(function (presetVarName) {
    fontPlayer.loader.decodeAfterLoading(audioContext, presetVarName)
  })
}
