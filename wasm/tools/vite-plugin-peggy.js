import peggy from 'peggy';

export const enforce = 'pre';

export default function peggyPlugin() {
  return {
    name: 'vite-peggy-plugin',
    enforce: 'pre',
    async transform(grammar, id) {
      if (!id.endsWith(".pegjs")) return;

      const code = peggy.generate(grammar, {
        output: "source",
        format: "es",
      });

      return {
        code
      };
    }
  };
}
