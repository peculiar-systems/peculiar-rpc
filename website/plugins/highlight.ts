type Token = "co" | "st" | "lb" | "kw" | "ty" | "nu";

const never = "(?!)";

const keywords: { readonly [language: string]: string } = {
  haskell:
    "module|import|qualified|as|where|data|newtype|type|class|instance|deriving|let|in|case|of|if|then|else|do|forall|family|stock|anyclass|via",
  nix: "let|in|inherit|with|rec|if|then|else",
  proto:
    "syntax|package|import|service|rpc|returns|stream|message|enum|option|repeated|optional|oneof|map",
  yaml: "true|false",
  shell: "nix|run|build|flake|check|grpcurl|curl|cabal|protoc",
};

const comments: { readonly [language: string]: string } = {
  haskell: String.raw`--[^\n]*|\{-#[\s\S]*?#-\}`,
  nix: String.raw`#[^\n]*`,
  proto: String.raw`//[^\n]*`,
  yaml: String.raw`#[^\n]*`,
  shell: String.raw`(?:^|\s)#[^\n]*`,
};

const typed = new Set(["haskell", "proto"]);

const rules = (language: string): readonly (readonly [Token, string])[] => {
  const words = keywords[language];
  return [
    ["co", comments[language] ?? never],
    ["st", String.raw`"(?:[^"\\\n]|\\.)*"`],
    ["lb", language === "haskell" ? String.raw`#[a-z][A-Za-z0-9_']*` : never],
    ["kw", words === undefined ? never : String.raw`\b(?:${words})\b`],
    [
      "ty",
      typed.has(language) ? String.raw`\b[A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*` : never,
    ],
    ["nu", String.raw`\b\d[\d_]*\b`],
  ];
};

export const escape = (text: string): string =>
  text
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");

const span = (table: readonly (readonly [Token, string])[], match: RegExpExecArray): string => {
  const kind = table.find(([name]) => match.groups?.[name] !== undefined)?.[0];
  return kind === undefined
    ? escape(match[0])
    : `<span class="t-${kind}">${escape(match[0])}</span>`;
};

export const highlight = (language: string, code: string): string => {
  const table = rules(language);
  const pattern = new RegExp(table.map(([name, rule]) => `(?<${name}>${rule})`).join("|"), "gu");
  const matches = [...code.matchAll(pattern)];
  const ends = matches.map((match) => match.index + match[0].length);
  const pieces = matches.map(
    (match, index) => escape(code.slice(ends[index - 1] ?? 0, match.index)) + span(table, match),
  );
  return pieces.join("") + escape(code.slice(ends.at(-1) ?? 0));
};
