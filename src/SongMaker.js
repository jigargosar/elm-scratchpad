import { Elm } from "./SongMaker.elm";
import * as Tone from "tone/build/Tone.js";

const AudioContextFunc = window.AudioContext || window["webkitAudioContext"];
const audioContext = new AudioContextFunc();
const player = new WebAudioFontPlayer();
const fileName = "_tone_" + "0000_SBLive_sf2";
player.loader.decodeAfterLoading(audioContext, fileName);

function playNote2(note, startTime) {
  player.queueWaveTable(
    audioContext,
    audioContext.destination,
    window[fileName],
    startTime,
    NoteParser.midi(note) - 12,
    // 2,
    // Tone.Time("10n").toSeconds(),
    0.3,
    0.5
  );
  return false;
}

const synths = {
  synth: new Tone.PolySynth(Tone.Synth).toDestination(),
  membraneSynth: new Tone.MembraneSynth().toDestination(),
  metalSynth: new Tone.MetalSynth().toDestination(),
  pluckSynth: new Tone.PolySynth(Tone.PluckSynth).toDestination(),
};
const app = Elm.SongMaker.init();

const Player = (function () {
  const noteDuration = 0.01;
  const noteGap = "8n";
  const bpm = 120;

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
    if (inst === "synth" && !inst) {
      playNote2(note, time ? time : 0);
    } else {
      synths[inst].triggerAttackRelease(note, noteDuration, time);
    }
  }

  function updateStepsAndInitSeqIfRequired(steps_) {
    steps = steps_;
    if (!seq) {
      seq = new Tone.Sequence(
        (time, i) => {
          Tone.Draw.schedule(function () {
            app.ports.selectColumn.send(i);
          }, time);
          // console.log(steps[i]);

          steps[i].forEach((data) => playNote(data, time));
        },
        steps.map((_, i) => i),
        noteGap
      );
      seq.start(0);
    }
  }

  return {
    updateSteps(steps_) {
      steps = steps_;
    },
    async play(steps_) {
      await Tone.start();
      updateStepsAndInitSeqIfRequired(steps_);
      Tone.Transport.start();
    },
    async toggle(steps_) {
      await Tone.start();
      updateStepsAndInitSeqIfRequired(steps_);
      Tone.Transport.toggle();
    },
    async playSingleNote(data) {
      await Tone.start();
      playNote(data);
    },
    stop() {
      Tone.Transport.stop();
    },
    pause() {
      Tone.Transport.pause();
    },
    get seq() {
      return seq;
    },
  };
})();

window.__Player = Player;
window.__Tone = Tone;

app.ports.play.subscribe(Player.play);
app.ports.togglePlay.subscribe(Player.toggle);
app.ports.updateSteps.subscribe(Player.updateSteps);
app.ports.playSingleNote.subscribe(Player.playSingleNote);
app.ports.stop.subscribe(Player.stop);
app.ports.pause.subscribe(Player.pause);
