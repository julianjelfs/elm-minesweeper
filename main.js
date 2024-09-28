import "./normalize.css";
import "./styles.css";
import { initialise } from "@open-ic/openchat-embed";
import { Elm } from "./src/Main.elm";

window.onload = async () => {
  let level = Number(localStorage.getItem("openchat_minesweeper_level") ?? 1);
  let username = "User";
  if (window.self !== window.top) {
    const client = await initialise();
    username = client.username;
  }
  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: { username: username, level },
  });

  app.ports.updateLevel.subscribe((level) => {
    localStorage.setItem("openchat_minesweeper_level", level.toString());
  });
};
