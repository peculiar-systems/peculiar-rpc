import { hydrate, prerender as ssr } from "preact-iso";
import { App } from "./app/App.tsx";
import { findPage } from "./content.ts";

const product = "peculiar-rpc";

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
    },
  };
};
