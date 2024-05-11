import { defineConfig } from 'vite';
import monacoEditorPlugin from 'vite-plugin-monaco-editor';

import peggyPlugin from '../tools/vite-plugin-peggy.js';

export default defineConfig({
  plugins: [
    peggyPlugin(),
    (monacoEditorPlugin.default || monacoEditorPlugin)({
      languageWorkers: ['editorWorkerService', 'json'],
    }),
  ],
  base: '/luau/playground/',
  build: {
    outDir: '../../docs/playground',
    emptyOutDir: true,
  },
});
