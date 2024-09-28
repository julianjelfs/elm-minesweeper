import "./normalize.css";
import "./styles.css";
import { initialise } from "@open-ic/openchat-embed";
import { Elm } from "./src/Main.elm";

window.onload = async () => {
  let username = "User";
  if (window.self !== window.top) {
    const client = await initialise();
    username = client.username;
  }
  Elm.Main.init({
    node: document.getElementById("app"),
    flags: { username: username },
  });
};
