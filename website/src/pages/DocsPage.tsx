import { prose, wrap } from "../app/global.css.ts";
import { docSections, findPage, neighbours } from "../content.ts";
import { NotFound } from "./NotFound.tsx";
import * as s from "./pages.css.ts";

const DocsNav = ({ current }: { readonly current: string }) => (
  <nav class={s.docsNav} aria-label="Documentation">
    {docSections().map((section) => (
      <div key={section.name}>
        <p class={s.docsSection}>{section.name}</p>
        <ul class={s.docsList}>
          {section.pages.map((page) => (
            <li key={page.path}>
              <a
                class={s.docsLink}
                href={page.path}
                aria-current={page.path === current ? "page" : undefined}
              >
                {page.title}
              </a>
            </li>
          ))}
        </ul>
      </div>
    ))}
  </nav>
);

export const DocsPage = ({ path }: { readonly path: string }) => {
  const page = findPage(path);
  if (page === undefined) {
    return <NotFound />;
  }
  const { previous, next } = neighbours(page);
  return (
    <div class={`${wrap} ${s.docsLayout}`}>
      <DocsNav current={page.path} />
      <article class={s.docsArticle}>
        <p class={s.kicker}>{page.section}</p>
        <div class={prose} dangerouslySetInnerHTML={{ __html: page.html }} />
        <nav class={s.pager} aria-label="Pages">
          {previous === undefined ? <span /> : <a href={previous.path}>← {previous.title}</a>}
          {next === undefined ? <span /> : <a href={next.path}>{next.title} →</a>}
        </nav>
      </article>
    </div>
  );
};
