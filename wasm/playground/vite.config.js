import { defineConfig } from 'vite';
import monacoEditorPlugin from 'vite-plugin-monaco-editor';

import peggyPlugin from '../tools/vite-plugin-peggy.js';

export default defineConfig({
  plugins: [
    peggyPlugin(),
    (monacoEditorPlugin.default || monacoEditorPlugin)({}),
  ],
});
