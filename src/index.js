require("./index.html");
import css from "./style.css";

const Elm = require("./Main.elm").Elm;
const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: {
    baseUrl: process.env.BASE_URL || "http://localhost:8000",
    version: process.env.VERSION || "dev"
  }
});

let lastObjectUrl;
const b64toBlob = (b64Data, contentType = "", sliceSize = 512) => {
  const byteCharacters = atob(b64Data);
  const byteArrays = [];

  for (let offset = 0; offset < byteCharacters.length; offset += sliceSize) {
    const slice = byteCharacters.slice(offset, offset + sliceSize);

    const byteNumbers = new Array(slice.length);
    for (let i = 0; i < slice.length; i++) {
      byteNumbers[i] = slice.charCodeAt(i);
    }

    const byteArray = new Uint8Array(byteNumbers);
    byteArrays.push(byteArray);
  }

  const blob = new Blob(byteArrays, { type: contentType });
  return blob;
};

app.ports.setVideo.subscribe(function(data) {
  if (lastObjectUrl) {
    URL.revokeObjectURL(lastObjectUrl);
  }

  let blob = b64toBlob(data, "video/mp4");
  let url = URL.createObjectURL(blob);
  document.getElementById("player").src = url;
  document.getElementById("download").href = url;
  lastObjectUrl = url;
});

app.ports.clearVideo.subscribe(function(data) {
  if (lastObjectUrl) {
    URL.revokeObjectURL(lastObjectUrl);
    lastObjectUrl = null;
  }

  let player = document.getElementById("player");
  player.pause();
  player.removeAttribute("src");
  document.getElementById("download").removeAttribute("href");
});
