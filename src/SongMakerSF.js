import { Elm } from "./SongMakerSF.elm"

const app = Elm.SongMakerSF.init()

const Player = MakePlayer()

window.Player ??= Player

app.ports.start.subscribe(Player.start)
app.ports.stop.subscribe(Player.stop)
app.ports.updateSteps.subscribe(Player.updateSteps)
app.ports.playSingleNote.subscribe(Player.playSingleNote)

function newAudioContext() {
  const AudioContextFunc = window.AudioContext || window["webkitAudioContext"]
  return new AudioContextFunc()
}

function loadPresets(audioContext, fontPlayer, presetNames) {
  presetNames.forEach(function (presetVarName) {
    fontPlayer.loader.decodeAfterLoading(audioContext, presetVarName)
  })
}

function MakePlayer() {
  const audioContext = newAudioContext()

  const fontPlayer = new WebAudioFontPlayer()
  const presetMap = {
    synth: "_tone_" + "0000_SBLive_sf2",
    drum: "_drum_35_0_Chaos_sf2_file",
  }
  const presetNames = Object.values(presetMap)
  loadPresets(audioContext, fontPlayer, presetNames)

  const bpm = 120
  const barLengthInSeconds = (4 * 60) / bpm
  const totalBars = 4
  console.log("barLengthInSeconds: ", barLengthInSeconds)
  const loopLengthInSeconds = totalBars * barLengthInSeconds
  console.log("loopLengthInSeconds:", loopLengthInSeconds)
  // const noteDuration = (1 / 16) * barLengthInSeconds;
  const noteGap = (1 / 8) * barLengthInSeconds
  const noteDuration = noteGap

  const ticker = new WebAudioFontTicker()

  let steps = null

  function playNote([presetName, note], at = 0) {
    const presetVar = window[presetMap[presetName]]
    fontPlayer.queueWaveTable(
      audioContext,
      audioContext.destination,
      presetVar,
      at,
      NoteParser.midi(note),
      noteDuration,
      0.5,
      [],
    )
  }

  function playLoop(
    player,
    audioContext,
    loopStart,
    loopPosition,
    loopEnd,
    steps_,
  ) {
    steps = steps_
    ticker.startTicks(
      audioContext,
      function (wallClock, from, to) {
        for (let i = 0; i < steps.length; i++) {
          const noteStartTimeInLoop = i * noteGap
          if (noteStartTimeInLoop >= from && noteStartTimeInLoop < to) {
            const scheduleDelay = noteStartTimeInLoop - from
            setTimeout(
              () => app.ports.selectColumn.send(i),
              scheduleDelay - 0.1,
            )
            const scheduleTime = wallClock + scheduleDelay
            steps[i].forEach((data) => playNote(data, scheduleTime))
          }
        }
      },
      loopStart,
      loopPosition,
      loopEnd,
      function () {
        player.cancelQueue(audioContext)
      },
    )
  }

  return {
    updateSteps(steps_) {
      steps = steps_
    },
    start(steps_) {
      ticker.cancel()
      playLoop(
        fontPlayer,
        audioContext,
        0,
        ticker.lastPosition, // 0,
        loopLengthInSeconds,
        steps_,
      )
    },
    stop() {
      ticker.cancel()
    },
    playSingleNote(data) {
      playNote(data)
    },
  }
}
