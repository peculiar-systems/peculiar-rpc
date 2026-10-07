import { style } from "@vanilla-extract/css";
import { vars } from "../app/theme.css.ts";

export const article = style({
  padding: "56px 0 72px",
});

export const docsLayout = style({
  display: "grid",
  gridTemplateColumns: "230px minmax(0, 1fr)",
  gap: "40px",
  padding: "48px 0 72px",
  "@media": { "(max-width: 800px)": { gridTemplateColumns: "minmax(0, 1fr)", gap: "24px" } },
});

export const docsNav = style({
  position: "sticky",
  top: "84px",
  alignSelf: "start",
  display: "flex",
  flexDirection: "column",
  gap: "18px",
  "@media": { "(max-width: 800px)": { position: "static" } },
});

export const docsSection = style({
  fontFamily: vars.font.mono,
  fontSize: "11px",
  letterSpacing: ".12em",
  textTransform: "uppercase",
  color: vars.color.inkFaint,
  marginBottom: "6px",
});

export const docsList = style({
  listStyle: "none",
  display: "flex",
  flexDirection: "column",
  gap: "2px",
  "@media": { "(max-width: 800px)": { flexDirection: "row", flexWrap: "wrap", gap: "6px" } },
});

export const docsLink = style({
  display: "block",
  padding: "6px 10px",
  borderRadius: "8px",
  fontSize: "14.5px",
  color: vars.color.inkSoft,
  selectors: {
    "&:hover": { background: vars.color.bg2, color: vars.color.ink },
    '&[aria-current="page"]': {
      background: vars.color.violetTint,
      color: vars.color.violetInk,
      fontWeight: 600,
    },
  },
});

export const kicker = style({
  fontFamily: vars.font.mono,
  fontSize: "12px",
  letterSpacing: ".14em",
  textTransform: "uppercase",
  color: vars.color.violetInk,
  marginBottom: "10px",
});

export const notFound = style({
  padding: "120px 0",
  textAlign: "center",
});

export const docsArticle = style({
  minWidth: 0,
});

export const pager = style({
  display: "flex",
  justifyContent: "space-between",
  gap: "14px",
  maxWidth: "72ch",
  marginTop: "48px",
  paddingTop: "22px",
  borderTop: `1px solid ${vars.color.line}`,
  fontSize: "14.5px",
  color: vars.color.violetInk,
});
