import { Elm } from "./SongMaker.elm";
import * as Tone from "tone/build/Tone.js";

const synth = new Tone.PolySynth().toDestination();
// synth.debug = true;
// const notesSubdivisionArray = [
//   "C4",
//   ["E4", "D4", "E4"],
//   "G4",
//   ["A4", "G4"],
// ];
// const notes = notesSubdivisionArray.flat();
// const noteIndices = notes.map((_, i) => i);
// const seq = new Tone.Sequence(
//   (time, i) => {
//     Tone.Draw.schedule(function () {app.ports.selectColumn.send(i)}, time);
//     synth.triggerAttackRelease(notes[i], "8n", time);
//     // synth.triggerAttackRelease(notes[i], 0.1, time+0.05);
//   },
//   noteIndices,
//   "4n"
// );
// seq.start(0);
const app = Elm.SongMaker.init();

const Player = (function () {
  const noteDuration = 0.1;

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

  Tone.Transport.bpm.value = 120;

  function updateStepsAndInitSeqIfRequired(steps_) {
    steps = steps_;
    if (!seq) {
      seq = new Tone.Sequence(
        (time, i) => {
          Tone.Draw.schedule(function () {
            app.ports.selectColumn.send(i);
          }, time);
          // console.log(steps[i]);
          synth.triggerAttackRelease(steps[i], noteDuration, time);
        },
        steps.map((_, i) => i),
        "4n"
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
    async playSingleNote(note) {
      await Tone.start();
      synth.triggerAttackRelease(note, noteDuration);
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
