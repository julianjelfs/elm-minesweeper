import "./normalize.css";
import "./styles.css";

import { Elm } from "./src/Main.elm";
Elm.Main.init({
  node: document.getElementById("app"),
});
