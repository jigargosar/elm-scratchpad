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

app.ports.play.subscribe(async function (steps) {
  await Tone.start();
  Tone.Transport.cancel(0);
  Tone.Transport.stop();
  Tone.Transport.bpm.value = 120;

  const seq = new Tone.Sequence(
    (time, i) => {
      Tone.Draw.schedule(function () {
        app.ports.selectColumn.send(i);
      }, time);
      console.log(steps[i])
      synth.triggerAttackRelease(steps[i], 0.1, time);
    },
    steps.map((_, i) => i),
    "4n"
  );
  seq.start(0);
  Tone.Transport.start();
});
app.ports.stop.subscribe(async function () {
  await Tone.start();
  Tone.Transport.stop();
});
