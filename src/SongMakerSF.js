import { Elm } from "./SongMakerSF.elm";
import * as Tone from "tone/build/Tone.js";

// console.log(NoteParser.midi("B1"))
// const AudioContextFunc = window.AudioContext || window["webkitAudioContext"];
// const audioContext = new AudioContextFunc();
const audioContext = Tone.getContext().rawContext._nativeAudioContext;

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
  const noteDuration = (1 / 8) * barLengthInSeconds;

  const ticker = new WebAudioFontTicker();

  let seq = null;
  let steps = null;
  let state = "unknown";

  function pollAndReportStateChange() {
    if (state !== Tone.Transport.state) {
      state = Tone.Transport.state;
      app.ports.stateChanged.send(state);
    }
  }

  pollAndReportStateChange();

  Tone.Transport.on("start", pollAndReportStateChange);
  Tone.Transport.on("stop", pollAndReportStateChange);
  Tone.Transport.on("pause", pollAndReportStateChange);

  Tone.Transport.bpm.value = bpm;

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

  // function updateStepsAndInitSeqIfRequired(steps_) {
  //   steps = steps_;
  //   if (!seq) {
  //     seq = new Tone.Sequence(
  //       (time, i) => {
  //         Tone.Draw.schedule(function () {
  //           app.ports.selectColumn.send(i);
  //         }, time);
  //         // console.log(steps[i]);
  //
  //         steps[i].forEach((data) => playNote(data, time));
  //       },
  //       steps.map((_, i) => i),
  //       noteGap
  //     );
  //     seq.start(0);
  //   }
  // }

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
      function (when, from, to) {
        for (let i = 0; i < steps.length; i++) {
          const noteWhen = i * noteDuration;
          if (noteWhen >= from && noteWhen < to) {
            const start = when + noteWhen - from;
            console.log(when, from, to);
            steps[i].forEach((data) => playNote(data, start));
            // playNote(steps[i], start);
            // player.queueWaveTable(audioContext, note.destination, note.preset, start, note.pitch, note.duration, note.volume, note.slides);
          }
        }
      },
      loopStart,
      loopPosition,
      loopEnd,
      function (at) {
        console.log("at", at);
        player.cancelQueue(audioContext);
      }
    );
  }

  return {
    updateSteps(steps_) {
      steps = steps_;
    },
    async toggle(steps_) {
      await Tone.start();
      // updateStepsAndInitSeqIfRequired(steps_);
      // Tone.Transport.toggle();
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
      await Tone.start();
      playNote(data);
    },
    get seq() {
      return seq;
    },
  };
})();

window.Player ??= Player;
window.Tone ??= Tone;

app.ports.togglePlay.subscribe(Player.toggle);
app.ports.updateSteps.subscribe(Player.updateSteps);
app.ports.playSingleNote.subscribe(Player.playSingleNote);
