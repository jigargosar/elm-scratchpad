import { Elm } from "./SongMakerSF.elm"

const app = Elm.SongMakerSF.init()

const Player = MakePlayer()

app.ports.scheduleNote?.subscribe(Player.scheduleNote)

function MakePlayer() {
  const audioContext = newAudioContext()

  const fontPlayer = new WebAudioFontPlayer()
  const presetMap = {
    piano: "_tone_0000_SBLive_sf2",
    strings: "_tone_0450_Chaos_sf2_file", //
    // bassDrum2: "_drum_35_0_Chaos_sf2_file",
    bassDrum1: "_drum_36_0_Chaos_sf2_file",
    snareDrum2: "_drum_40_0_Chaos_sf2_file",
    // highWoodBlock: "_drum_76_0_Chaos_sf2_file",
    // lowWoodBlock: "_drum_77_0_Chaos_sf2_file",
    highWoodBlock: "_drum_76_0_JCLive_sf2_file",
    lowWoodBlock: "_drum_77_0_JCLive_sf2_file",
  }
  const presetNames = Object.values(presetMap)
  loadPresets(audioContext, fontPlayer, presetNames)

  function presetFromName(presetName) {
    const presetVarName = presetMap[presetName]
    if (presetVarName == null) {
      throw new Error("Unknown preset name: " + presetName)
    }
    const preset = window[presetVarName]
    if (preset == null) {
      throw new Error(`Unable to load preset: ${presetName}: ${presetVarName}`)
    }
    return preset
  }

  const scheduleDelay = 100

  setInterval(function () {
    // requestAnimationFrame(() => {
    app.ports.onAudioContextTime?.send(audioContext.currentTime * 1000)
    // })
  }, scheduleDelay / 2)

  return {
    async scheduleNote({
      preset: presetName,
      atAudioTime: atAudioTimeInMilli,
      pitch: midiPitch,
      duration: durationInMilli,
    }) {
      const preset = presetFromName(presetName)
      if (!preset) {
        throw new Error("Invalid preset name: " + presetName)
      }
      await audioContext.resume()
      fontPlayer.queueWaveTable(
        audioContext,
        audioContext.destination,
        preset,
        atAudioTimeInMilli / 1000,
        // NoteParser.midi(midiPitch),
        midiPitch,
        durationInMilli / 1000,
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
