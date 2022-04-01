import { Elm } from "./SongMakerSF.elm"

const app = Elm.SongMakerSF.init()

const Player = MakePlayer()

app.ports.scheduleNote?.subscribe(Player.scheduleNote)

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

  const scheduleDelay = 100

  setInterval(function () {
    // requestAnimationFrame(() => {
    app.ports.onAudioContextTime?.send(audioContext.currentTime * 1000)
    // })
  }, scheduleDelay / 2)

  return {
    async scheduleNote({ preset, atAudioTime, pitch, duration }) {
      await audioContext.resume()
      const presetVar = window[presetMap[preset]]
      if (!presetVar) {
        throw new Error("Invalid preset name: " + preset)
      }
      fontPlayer.queueWaveTable(
        audioContext,
        audioContext.destination,
        presetVar,
        atAudioTime / 1000,
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
