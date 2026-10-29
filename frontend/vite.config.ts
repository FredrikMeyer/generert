import { defineConfig, type Plugin } from "vite";
import react from "@vitejs/plugin-react";
import { parseEDNString } from "edn-data";
import fs from "node:fs";
import path from "node:path";

const imagesDir = path.resolve(__dirname, "../images");
const virtualId = "virtual:published-images";
const resolvedVirtualId = "\0" + virtualId;

function findEdnFiles(dir: string): string[] {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) return findEdnFiles(fullPath);
    return entry.name.endsWith(".edn") ? [fullPath] : [];
  });
}

type Metadata = { title: string; description: string; published: string };

// Exposes every image in ../images that has an .edn file next to it.
// Only those PNGs end up in the build.
function publishedImages(): Plugin {
  return {
    name: "published-images",
    resolveId(id) {
      if (id === virtualId) return resolvedVirtualId;
    },
    load(id) {
      if (id !== resolvedVirtualId) return;
      const entries = findEdnFiles(imagesDir).flatMap((ednPath) => {
        const pngPath = ednPath.replace(/\.edn$/, ".png");
        if (!fs.existsSync(pngPath)) {
          this.warn(`No PNG found for ${ednPath}`);
          return [];
        }
        this.addWatchFile(ednPath);
        const meta = parseEDNString(fs.readFileSync(ednPath, "utf-8"), {
          mapAs: "object",
          keywordAs: "string",
        }) as Metadata;
        return [{ ...meta, pngPath }];
      });
      entries.sort((a, b) => b.published.localeCompare(a.published));

      const imports = entries.map(
        (e, i) => `import url${i} from ${JSON.stringify(e.pngPath)};`,
      );
      const items = entries.map(
        (e, i) =>
          `{ name: ${JSON.stringify(e.title)}, description: ${JSON.stringify(e.description)}, published: ${JSON.stringify(e.published)}, url: url${i} }`,
      );
      return `${imports.join("\n")}\nexport default [${items.join(",\n")}];`;
    },
    configureServer(server) {
      server.watcher.add(imagesDir);
      const reload = (file: string) => {
        if (!file.startsWith(imagesDir) || !/\.(edn|png)$/.test(file)) return;
        const mod = server.moduleGraph.getModuleById(resolvedVirtualId);
        if (mod) server.moduleGraph.invalidateModule(mod);
        server.ws.send({ type: "full-reload" });
      };
      server.watcher.on("add", reload);
      server.watcher.on("change", reload);
      server.watcher.on("unlink", reload);
    },
  };
}

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), publishedImages()],
});
