import "./normalize.css";
import "./styles.css";
import { initialise } from "@open-ic/openchat-embed";
import { Elm } from "./src/Main.elm";

window.onload = async () => {
  const client = await initialise();
  Elm.Main.init({
    node: document.getElementById("app"),
    flags: { username: client.username },
  });
};
