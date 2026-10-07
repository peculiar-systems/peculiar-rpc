import preact from "@preact/preset-vite";
import { vanillaExtractPlugin } from "@vanilla-extract/vite-plugin";
import { defineConfig } from "vite";
import { markdown } from "./plugins/markdown.ts";

export default defineConfig({
  plugins: [
    markdown(),
    vanillaExtractPlugin(),
    preact({
      prerender: {
        enabled: true,
        renderTarget: "#app",
        additionalPrerenderRoutes: ["/404"],
      },
    }),
  ],
  build: {
    target: "es2022",
  },
});
