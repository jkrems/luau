import { defineConfig } from "vite";

import peggyPlugin from "./tools/vite-plugin-peggy.js";

export default defineConfig({
  plugins: [peggyPlugin()],
});
