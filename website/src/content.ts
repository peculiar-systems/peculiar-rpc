type Document = {
  readonly meta: { readonly [key: string]: string };
  readonly html: string;
};

export type Page = {
  readonly path: string;
  readonly title: string;
  readonly order: number;
  readonly section: string;
  readonly html: string;
};

const docs = import.meta.glob<Document>("../content/docs/*.md", { eager: true });

const base = "../content/docs/";

const toPage = (file: string, document: Document): Page => {
  const name = file.slice(base.length).replace(/\.md$/u, "");
  const order = Number(document.meta["order"] ?? "999");
  return {
    path: `/docs/${name}`,
    title: document.meta["title"] ?? name,
    order: Number.isNaN(order) ? 999 : order,
    section: document.meta["section"] ?? "",
    html: document.html,
  };
};

export const docPages: readonly Page[] = Object.entries(docs)
  .map(([file, document]) => toPage(file, document))
  .toSorted((a, b) => (a.order === b.order ? a.title.localeCompare(b.title) : a.order - b.order));

export const findPage = (path: string): Page | undefined => {
  const wanted = path.replace(/\/$/u, "");
  return wanted === "/docs" ? docPages[0] : docPages.find((page) => page.path === wanted);
};

export const neighbours = (
  page: Page,
): { readonly previous: Page | undefined; readonly next: Page | undefined } => {
  const index = docPages.indexOf(page);
  return { previous: docPages[index - 1], next: docPages[index + 1] };
};

export const docSections = (): readonly {
  readonly name: string;
  readonly pages: readonly Page[];
}[] => {
  const names = [...new Set(docPages.map((page) => page.section))];
  return names.map((name) => ({ name, pages: docPages.filter((page) => page.section === name) }));
};
