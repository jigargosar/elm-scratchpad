import { Elm } from "./SongMaker.elm";
import * as Tone from "tone/build/Tone.js";

// console.log(NoteParser.midi("B1"))
// const AudioContextFunc = window.AudioContext || window["webkitAudioContext"];
// const audioContext = new AudioContextFunc();
const audioContext = Tone.getContext().rawContext._nativeAudioContext;

const fontPlayer = new WebAudioFontPlayer();
const synth2Name = "_tone_" + "0000_SBLive_sf2";
fontPlayer.loader.decodeAfterLoading(audioContext, synth2Name);
const bassDrum2Name = "_drum_35_0_Chaos_sf2_file"
fontPlayer.loader.decodeAfterLoading(audioContext, bassDrum2Name)
const synths = {
  synth2: synth2Name,
  drum2: bassDrum2Name,
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
    const synth = synths[inst];
    if (typeof synth === 'string') {
      playSoundFont(synth, note, time);
    } else {
      synth.triggerAttackRelease(note, noteDuration, time);
    }
  }

  function playSoundFont(preset, note, startTime = 0) {
    const ac = Tone.context._context._nativeAudioContext;
    fontPlayer.queueWaveTable(
      ac,
      ac.destination,
      window[preset],
      startTime,
      NoteParser.midi(note),
      Tone.Time("8n").toSeconds(),
      0.5
    );
    return false;
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

window.Player ??= Player;
window.Tone ??= Tone;

app.ports.play.subscribe(Player.play);
app.ports.togglePlay.subscribe(Player.toggle);
app.ports.updateSteps.subscribe(Player.updateSteps);
app.ports.playSingleNote.subscribe(Player.playSingleNote);
app.ports.stop.subscribe(Player.stop);
app.ports.pause.subscribe(Player.pause);
