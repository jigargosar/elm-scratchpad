import { Elm } from "./SongMakerSF.elm";
// import * as Tone from "tone/build/Tone.js";

// console.log(NoteParser.midi("B1"))
const AudioContextFunc = window.AudioContext || window["webkitAudioContext"];
const audioContext = new AudioContextFunc();
// const audioContext = Tone.getContext().rawContext._nativeAudioContext;

const fontPlayer = new WebAudioFontPlayer();
const synth2Name = "_tone_" + "0000_SBLive_sf2";
fontPlayer.loader.decodeAfterLoading(audioContext, synth2Name);
const bassDrum2Name = "_drum_35_0_Chaos_sf2_file";
fontPlayer.loader.decodeAfterLoading(audioContext, bassDrum2Name);
const synths = {
  synth: synth2Name,
  drum: bassDrum2Name,
};
const app = Elm.SongMakerSF.init();

const Player = (function () {
  const bpm = 120;
  const barLengthInSeconds = (4 * 60) / bpm;
  const totalBars = 4;
  console.log("barLengthInSeconds: ", barLengthInSeconds);
  const loopLengthInSeconds = totalBars * barLengthInSeconds;
  console.log("loopLengthInSeconds:", loopLengthInSeconds);
  // const noteDuration = (1 / 16) * barLengthInSeconds;
  const noteGap = (1 / 8) * barLengthInSeconds;
  const noteDuration = noteGap;

  const ticker = new WebAudioFontTicker();

  // let seq = null;
  let steps = null;
  // let state = "unknown";

  // function pollAndReportStateChange() {
  //   if (state !== Tone.Transport.state) {
  //     state = Tone.Transport.state;
  //     app.ports.stateChanged.send(state);
  //   }
  // }
  //
  // pollAndReportStateChange();
  //
  // Tone.Transport.on("start", pollAndReportStateChange);
  // Tone.Transport.on("stop", pollAndReportStateChange);
  // Tone.Transport.on("pause", pollAndReportStateChange);
  //
  // Tone.Transport.bpm.value = bpm;

  function playNote([inst, note], time) {
    const synth = synths[inst];
    playSoundFont(synth, note, time);
  }

  function playSoundFont(preset, note, startTime = 0) {
    fontPlayer.queueWaveTable(
      audioContext,
      audioContext.destination,
      window[preset],
      startTime,
      NoteParser.midi(note),
      // Tone.Time("8n").toSeconds(),
      noteDuration,
      0.5
    );
    return false;
  }

  function playLoop(
    player,
    audioContext,
    loopStart,
    loopPosition,
    loopEnd,
    steps_
  ) {
    steps = steps_;
    ticker.startTicks(
      audioContext,
      function (wallClock, from, to) {
        for (let i = 0; i < steps.length; i++) {
          const noteStartTimeInLoop = i * noteGap;
          if (noteStartTimeInLoop >= from && noteStartTimeInLoop < to) {
            const scheduleDelay = noteStartTimeInLoop - from;
            setTimeout(() => app.ports.selectColumn.send(i), scheduleDelay);
            const scheduleTime = wallClock + scheduleDelay;
            steps[i].forEach((data) => playNote(data, scheduleTime));
          }
        }
      },
      loopStart,
      loopPosition,
      loopEnd,
      function () {
        player.cancelQueue(audioContext);
      }
    );
  }

  return {
    updateSteps(steps_) {
      steps = steps_;
    },
    async toggle(steps_) {
      if (ticker.state === ticker.statePlay) {
        ticker.cancel();
      } else {
        playLoop(
          fontPlayer,
          audioContext,
          0,
          ticker.lastPosition,
          loopLengthInSeconds,
          steps_
        );
      }
    },
    async playSingleNote(data) {
      playNote(data);
    },
  };
})();

window.Player ??= Player;

app.ports.togglePlay.subscribe(Player.toggle);
app.ports.updateSteps.subscribe(Player.updateSteps);
app.ports.playSingleNote.subscribe(Player.playSingleNote);
