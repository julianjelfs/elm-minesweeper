import "./normalize.css";
import "./styles.css";
import { initialise } from "@open-ic/openchat-embed";
import { Elm } from "./src/Main.elm";

window.onload = async () => {
  let level = Number(localStorage.getItem("openchat_minesweeper_level") ?? 1);
  let instructions =
    localStorage.getItem("openchat_minesweeper_instructions") !== "false";
  let username = "User";
  if (window.self !== window.top) {
    const client = await initialise();
    username = client.username;
  }

  const flags = {
    username: username,
    level,
    instructions,
  };

  console.log("Flags: ", flags);
  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags,
  });

  app.ports.updateLevel.subscribe((level) => {
    localStorage.setItem("openchat_minesweeper_level", level.toString());
  });

  app.ports.instructions.subscribe((show) => {
    localStorage.setItem("openchat_minesweeper_instructions", show.toString());
  });

  window.onresize = () => app.ports.resize.send(true);
};
