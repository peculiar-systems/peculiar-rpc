import { hydrate, prerender as ssr } from "preact-iso";
import { App } from "./app/App.tsx";
import { findPage } from "./content.ts";

const product = "peculiar-rpc";

const site = "https://rpc.peculiar.systems";
const description =
  "A Haskell library that serves native gRPC, gRPC-Web and Connect from one process on one port, with typed servers and clients derived from your .proto files.";

const pathOf = (url: string): string => {
  const trimmed = url.replace(/[?#].*$/u, "").replace(/\/$/u, "");
  return trimmed === "" ? "/" : trimmed;
};

type HeadElement = {
  readonly type: string;
  readonly props: { readonly [key: string]: string };
};

const headFor = (url: string, title: string): Set<HeadElement> => {
  const path = pathOf(url);
  if (path === "/404") {
    return new Set([{ type: "meta", props: { name: "robots", content: "noindex" } }]);
  }
  const address = `${site}${path === "/" ? "/" : path}`;
  return new Set<HeadElement>([
    { type: "link", props: { rel: "canonical", href: address } },
    { type: "meta", props: { property: "og:type", content: "website" } },
    { type: "meta", props: { property: "og:site_name", content: "Peculiar Systems" } },
    { type: "meta", props: { property: "og:title", content: title } },
    { type: "meta", props: { property: "og:description", content: description } },
    { type: "meta", props: { property: "og:url", content: address } },
    { type: "meta", props: { property: "og:image", content: `${site}/og.png` } },
    { type: "meta", props: { name: "twitter:card", content: "summary_large_image" } },
  ]);
};

const titleFor = (url: string): string => {
  if (url === "/" || url === "") {
    return `${product} · gRPC, gRPC-Web and Connect from one Haskell binary`;
  }
  const page = url.startsWith("/docs") ? findPage(url) : undefined;
  return page === undefined ? product : `${page.title} · ${product}`;
};

if ("document" in globalThis) {
  const root = globalThis.document.querySelector("#app");
  if (root !== null) {
    hydrate(<App />, root);
  }
}

export const prerender = async (data: { readonly url: string }) => {
  const { html, links } = await ssr(<App />);
  return {
    html,
    links,
    head: {
      title: titleFor(data.url),
      lang: "en",
      elements: headFor(data.url, titleFor(data.url)),
    },
  };
};
