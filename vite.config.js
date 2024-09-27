import { defineConfig } from "vite";
import elm from "vite-plugin-elm";

export default defineConfig({
  plugins: [elm()],
  build: {
    outDir: "dist", // Adjust based on your project structure
  },
});
