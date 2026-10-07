import { globalStyle, style } from "@vanilla-extract/css";
import { vars } from "./theme.css.ts";

globalStyle("*", { boxSizing: "border-box", margin: 0, padding: 0 });

globalStyle("html", { scrollBehavior: "smooth" });

globalStyle("body", {
  background: vars.color.bg,
  color: vars.color.ink,
  fontFamily: vars.font.body,
  fontSize: "17px",
  lineHeight: 1.6,
  WebkitFontSmoothing: "antialiased",
  textRendering: "optimizeLegibility",
  minHeight: "100dvh",
  display: "flex",
  flexDirection: "column",
  overflowX: "hidden",
});

globalStyle("::selection", { background: vars.color.violetTint2 });

globalStyle("a", { color: "inherit", textDecoration: "none" });

globalStyle(":focus-visible", {
  outline: `2.5px solid ${vars.color.violet}`,
  outlineOffset: "3px",
  borderRadius: "5px",
});

globalStyle("code, pre, kbd", { fontFamily: vars.font.mono });

globalStyle("h1, h2, h3, h4", {
  fontFamily: vars.font.display,
  fontWeight: 800,
  letterSpacing: "-0.02em",
  lineHeight: 1.05,
});

globalStyle("h3, h4", { fontWeight: 700 });

globalStyle("button", {
  fontFamily: "inherit",
  cursor: "pointer",
  border: "none",
  background: "none",
  color: "inherit",
});

export const wrap = style({
  width: "100%",
  maxWidth: vars.maxw,
  margin: "0 auto",
  padding: "0 28px",
  "@media": {
    "(max-width: 480px)": { padding: "0 16px" },
  },
});

export const visuallyHidden = style({
  position: "absolute",
  width: "1px",
  height: "1px",
  overflow: "hidden",
  clip: "rect(0 0 0 0)",
  clipPath: "inset(50%)",
  whiteSpace: "nowrap",
});

export const skip = style({
  position: "absolute",
  left: "-9999px",
  top: 0,
  zIndex: 200,
  background: vars.color.ink,
  color: "#fff",
  padding: "10px 18px",
  borderRadius: "0 0 10px 0",
  fontSize: "14px",
  selectors: {
    "&:focus": { left: 0 },
  },
});

export const button = style({
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  gap: "9px",
  padding: "13px 20px",
  borderRadius: "12px",
  fontSize: "15.5px",
  fontWeight: 500,
  whiteSpace: "nowrap",
  background: vars.color.lime,
  color: vars.color.ink,
  boxShadow: "0 8px 20px -10px oklch(0.84 0.2 124 / 0.7)",
  transition: "transform .15s ease, background .2s ease",
  selectors: {
    "&:hover": { transform: "translateY(-2px)", background: vars.color.limeStrong },
  },
});

export const buttonGhost = style({
  display: "inline-flex",
  alignItems: "center",
  justifyContent: "center",
  gap: "9px",
  padding: "13px 20px",
  borderRadius: "12px",
  fontSize: "15.5px",
  fontWeight: 500,
  whiteSpace: "nowrap",
  background: vars.color.surface,
  color: vars.color.ink,
  border: `1px solid ${vars.color.lineStrong}`,
  boxShadow: vars.shadowSm,
  transition: "transform .15s ease, border-color .2s ease",
  selectors: {
    "&:hover": { transform: "translateY(-2px)", borderColor: vars.color.ink },
  },
});

export const sectionTag = style({
  fontFamily: vars.font.mono,
  fontSize: "12px",
  letterSpacing: ".14em",
  textTransform: "uppercase",
  color: vars.color.inkFaint,
});

export const sectionTitle = style({
  fontSize: "clamp(26px, 4vw, 36px)",
  marginTop: "12px",
});

export const lede = style({
  marginTop: "12px",
  fontSize: "16.5px",
  color: vars.color.inkSoft,
  maxWidth: "62ch",
});

export const prose = style({
  maxWidth: "72ch",
  minWidth: 0,
});

globalStyle(`${prose} h1`, { fontSize: "clamp(32px, 5vw, 44px)", marginBottom: "18px" });
globalStyle(`${prose} h2`, {
  fontSize: "24px",
  marginTop: "44px",
  marginBottom: "12px",
  paddingTop: "18px",
  borderTop: `1px solid ${vars.color.line}`,
});
globalStyle(`${prose} h3`, { fontSize: "18px", marginTop: "28px", marginBottom: "8px" });
globalStyle(`${prose} p`, { margin: "0 0 14px", color: vars.color.inkSoft });
globalStyle(`${prose} p:first-of-type`, { color: vars.color.ink, fontSize: "18px" });
globalStyle(`${prose} ul, ${prose} ol`, {
  margin: "0 0 16px",
  paddingLeft: "22px",
  color: vars.color.inkSoft,
});
globalStyle(`${prose} li`, { marginBottom: "6px" });
globalStyle(`${prose} li::marker`, { color: vars.color.violetInk });
globalStyle(`${prose} strong`, { color: vars.color.ink, fontWeight: 600 });
globalStyle(`${prose} a`, {
  color: vars.color.violetInk,
  textDecoration: "underline",
  textUnderlineOffset: "2px",
});
globalStyle(`${prose} a:hover`, { color: vars.color.violetHover });
globalStyle(`${prose} :not(pre) > code`, {
  fontSize: "0.9em",
  background: vars.color.bg2,
  border: `1px solid ${vars.color.line}`,
  padding: "1px 6px",
  borderRadius: "6px",
  overflowWrap: "anywhere",
  color: vars.color.ink,
});
globalStyle(`${prose} td code, ${prose} th code`, { overflowWrap: "normal", whiteSpace: "nowrap" });

globalStyle(`${prose} pre`, {
  background: vars.color.codeBg,
  color: vars.color.codeInk,
  borderRadius: "12px",
  padding: "16px 18px",
  fontSize: "13.5px",
  lineHeight: 1.55,
  overflow: "auto",
  margin: "0 0 18px",
});
globalStyle(`${prose} pre code`, { fontFamily: "inherit" });
globalStyle(`${prose} table`, {
  width: "100%",
  borderCollapse: "collapse",
  fontSize: "14.5px",
  margin: "0 0 20px",
  display: "block",
  overflowX: "auto",
});
globalStyle(`${prose} th, ${prose} td`, {
  textAlign: "left",
  padding: "10px 12px",
  borderBottom: `1px solid ${vars.color.line}`,
  verticalAlign: "top",
});
globalStyle(`${prose} thead th`, {
  fontFamily: vars.font.mono,
  fontSize: "12px",
  letterSpacing: ".08em",
  textTransform: "uppercase",
  color: vars.color.inkFaint,
  fontWeight: 500,
  background: vars.color.bg2,
});
globalStyle(`${prose} blockquote`, {
  borderLeft: `3px solid ${vars.color.violet}`,
  padding: "4px 0 4px 18px",
  margin: "0 0 16px",
  color: vars.color.inkSoft,
});
globalStyle(`${prose} hr`, {
  border: "none",
  borderTop: `1px solid ${vars.color.line}`,
  margin: "28px 0",
});

globalStyle("pre .file", {
  display: "block",
  margin: "-2px 0 12px",
  fontSize: "11.5px",
  letterSpacing: ".06em",
  color: "oklch(0.7 0.03 280)",
});
globalStyle(".t-kw", { color: "oklch(0.78 0.13 300)" });
globalStyle(".t-ty", { color: "oklch(0.84 0.1 200)" });
globalStyle(".t-st", { color: "oklch(0.85 0.14 124)" });
globalStyle(".t-co", { color: "oklch(0.62 0.03 280)", fontStyle: "italic" });
globalStyle(".t-nu", { color: "oklch(0.83 0.12 60)" });
globalStyle(".t-lb", { color: "oklch(0.82 0.12 350)" });
globalStyle(`${lede} code`, {
  fontSize: "0.9em",
  background: vars.color.surface,
  border: `1px solid ${vars.color.line}`,
  padding: "1px 6px",
  borderRadius: "6px",
});
