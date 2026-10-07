import { Marked } from "marked";
import type { Plugin } from "vite";
import { escape, highlight } from "./highlight.ts";

type FrontMatter = { readonly [key: string]: string };

const parseFrontMatter = (
  source: string,
): { readonly meta: FrontMatter; readonly body: string } => {
  const match = /^---\n([\s\S]*?)\n---\n/u.exec(source);
  if (match === null || match[1] === undefined) {
    return { meta: {}, body: source };
  }
  const meta = Object.fromEntries(
    match[1]
      .split("\n")
      .map((line) => line.split(/:\s(.*)/u))
      .filter((parts): parts is [string, string, string] => parts.length >= 2)
      .map(([key, value]) => [key.trim(), value.trim()]),
  );
  return { meta, body: source.slice(match[0].length) };
};

const slug = (html: string): string =>
  html
    .replaceAll(/<[^>]*>/gu, "")
    .replaceAll(/&[a-z]+;/gu, "")
    .toLowerCase()
    .replaceAll(/[^a-z0-9]+/gu, "-")
    .replaceAll(/^-|-$/gu, "");

const marked = new Marked({
  gfm: true,
  async: false,
  renderer: {
    code: ({ text, lang }) => {
      const [language = "", ...caption] = (lang ?? "").trim().split(/\s+/u);
      const file =
        caption.length === 0 ? "" : `<span class="file">${escape(caption.join(" "))}</span>`;
      return `<pre>${file}<code>${highlight(language, text)}</code></pre>\n`;
    },
  },
});

const anchored = (html: string): string =>
  html.replaceAll(
    /<h([23])>([\s\S]*?)<\/h\1>/gu,
    (_, depth: string, inner: string) => `<h${depth} id="${slug(inner)}">${inner}</h${depth}>`,
  );

export const markdown = (): Plugin => ({
  name: "peculiar-markdown",
  enforce: "pre",
  transform: (code, id) => {
    if (!id.endsWith(".md")) {
      return null;
    }
    const { meta, body } = parseFrontMatter(code);
    const html = anchored(marked.parse(body, { async: false }));
    return {
      code: `export const meta = ${JSON.stringify(meta)};\nexport const html = ${JSON.stringify(html)};`,
      map: null,
    };
  },
});
