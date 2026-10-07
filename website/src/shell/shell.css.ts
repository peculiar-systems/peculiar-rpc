import { style } from "@vanilla-extract/css";
import { vars } from "../app/theme.css.ts";

export const nav = style({
  position: "sticky",
  top: 0,
  zIndex: 50,
  background: "oklch(0.991 0.006 95 / 0.82)",
  backdropFilter: "saturate(1.4) blur(12px)",
  borderBottom: `1px solid ${vars.color.line}`,
});

export const navInner = style({
  display: "flex",
  alignItems: "center",
  justifyContent: "space-between",
  gap: "12px",
  height: "64px",
});

export const brand = style({
  fontFamily: vars.font.display,
  fontWeight: 700,
  fontSize: "18px",
  letterSpacing: "-0.025em",
  whiteSpace: "nowrap",
});

export const brandMark = style({
  fontStyle: "normal",
  color: vars.color.magentaStrong,
});

export const navRight = style({
  display: "flex",
  alignItems: "center",
  gap: "14px",
});

export const navProduct = style({
  fontFamily: vars.font.display,
  fontWeight: 600,
  fontSize: "15px",
  color: vars.color.inkSoft,
  "@media": { "(max-width: 440px)": { display: "none" } },
});

export const navLink = style({
  fontSize: "14px",
  color: vars.color.inkSoft,
  transition: "color .18s",
  selectors: { "&:hover": { color: vars.color.ink } },
  "@media": { "(max-width: 640px)": { display: "none" } },
});

export const navDocs = style({
  fontSize: "14px",
  color: vars.color.inkSoft,
  transition: "color .18s",
  selectors: { "&:hover": { color: vars.color.ink } },
});

export const navBack = style([
  navLink,
  { "@media": { "(max-width: 560px)": { display: "none" } } },
]);

export const status = style({
  display: "inline-flex",
  alignItems: "center",
  gap: "7px",
  fontFamily: vars.font.mono,
  fontSize: "11px",
  letterSpacing: ".04em",
  padding: "5px 11px",
  borderRadius: "999px",
  border: `1px solid ${vars.color.lineStrong}`,
  color: vars.color.inkSoft,
  background: vars.color.surface,
  whiteSpace: "nowrap",
  "@media": { "(max-width: 360px)": { display: "none" } },
});

export const statusDot = style({
  width: "6px",
  height: "6px",
  borderRadius: "50%",
  background: vars.color.violet,
  boxShadow: `0 0 0 3px ${vars.color.violetTint2}`,
});

export const main = style({
  flex: 1,
});

export const footer = style({
  borderTop: `1px solid ${vars.color.line}`,
  padding: "26px 0",
  fontFamily: vars.font.mono,
  fontSize: "12.5px",
  color: vars.color.inkFaint,
  marginTop: "auto",
});

export const footerInner = style({
  display: "flex",
  flexWrap: "wrap",
  gap: "12px",
  justifyContent: "space-between",
  alignItems: "center",
});

export const footerLink = style({
  color: vars.color.violetInk,
  textDecoration: "underline",
  textUnderlineOffset: "2px",
});
