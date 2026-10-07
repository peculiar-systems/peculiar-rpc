import { globalStyle, style } from "@vanilla-extract/css";
import { vars } from "../app/theme.css.ts";

export const hero = style({
  position: "relative",
  padding: "96px 0 56px",
  overflow: "hidden",
});

export const gridBg = style({
  position: "absolute",
  inset: 0,
  zIndex: 0,
  pointerEvents: "none",
  backgroundImage: `linear-gradient(${vars.color.line} 1px, transparent 1px), linear-gradient(90deg, ${vars.color.line} 1px, transparent 1px)`,
  backgroundSize: "64px 64px",
  maskImage: "radial-gradient(120% 90% at 80% 0%, #000 0%, transparent 62%)",
  opacity: 0.6,
});

export const inner = style({
  position: "relative",
  zIndex: 1,
  maxWidth: "800px",
});

export const catTag = style({
  display: "inline-flex",
  alignItems: "center",
  gap: "9px",
  fontFamily: vars.font.mono,
  fontSize: "12px",
  letterSpacing: ".12em",
  textTransform: "uppercase",
  color: vars.color.inkSoft,
});

export const catDot = style({
  width: "9px",
  height: "9px",
  borderRadius: "50%",
  background: vars.color.violet,
});

export const title = style({
  fontSize: "clamp(40px, 8vw, 80px)",
  margin: "18px 0 0",
});

export const titleDot = style({
  color: vars.color.violet,
});

export const tagline = style({
  fontFamily: vars.font.display,
  fontWeight: 500,
  fontSize: "clamp(20px, 3vw, 30px)",
  letterSpacing: "-0.015em",
  marginTop: "18px",
  maxWidth: "30ch",
});

export const blurb = style({
  marginTop: "20px",
  fontSize: "18px",
  color: vars.color.inkSoft,
  maxWidth: "60ch",
});

globalStyle(`${blurb} b`, { color: vars.color.ink, fontWeight: 600 });

globalStyle(`${blurb} code`, {
  fontSize: "0.9em",
  background: vars.color.violetTint,
  padding: "1px 6px",
  borderRadius: "6px",
  color: vars.color.violetInk,
});

export const ctas = style({
  display: "flex",
  flexWrap: "wrap",
  gap: "12px",
  marginTop: "28px",
});

export const trust = style({
  marginTop: "18px",
  fontFamily: vars.font.mono,
  fontSize: "12.5px",
  color: vars.color.inkFaint,
  letterSpacing: ".01em",
});

export const heroCode = style({
  position: "relative",
  zIndex: 1,
  maxWidth: "800px",
  marginTop: "36px",
  background: vars.color.codeBg,
  color: vars.color.codeInk,
  borderRadius: "14px",
  padding: "20px 22px",
  fontSize: "13.5px",
  lineHeight: 1.65,
  overflow: "auto",
  fontFamily: vars.font.mono,
  boxShadow: vars.shadowSm,
});

export const section = style({
  padding: "56px 0",
});

export const sectionAlt = style([
  section,
  {
    background: vars.color.bg2,
    borderTop: `1px solid ${vars.color.line}`,
    borderBottom: `1px solid ${vars.color.line}`,
  },
]);

export const cards = style({
  display: "grid",
  gridTemplateColumns: "repeat(auto-fit, minmax(230px, 1fr))",
  gap: "14px",
  marginTop: "30px",
});

export const card = style({
  background: vars.color.surface,
  border: `1px solid ${vars.color.line}`,
  borderRadius: "14px",
  padding: "20px",
  boxShadow: vars.shadowSm,
});

export const cardKey = style({
  fontFamily: vars.font.mono,
  fontSize: "12px",
  color: vars.color.violetInk,
});

export const cardTitle = style({
  fontSize: "18px",
  margin: "8px 0 6px",
});

export const cardText = style({
  fontSize: "14.5px",
  color: vars.color.inkSoft,
});

globalStyle(`${cardText} code`, {
  fontSize: "13px",
  background: vars.color.bg2,
  padding: "1px 5px",
  borderRadius: "5px",
});

export const number = style({
  fontFamily: vars.font.display,
  fontWeight: 800,
  fontSize: "30px",
  letterSpacing: "-0.02em",
  color: vars.color.violetInk,
  fontVariantNumeric: "tabular-nums",
  marginBottom: "6px",
});

export const compare = style({
  display: "grid",
  gridTemplateColumns: "repeat(auto-fit, minmax(290px, 1fr))",
  gap: "14px",
  marginTop: "30px",
});

export const panel = style([card, { display: "flex", flexDirection: "column", gap: "12px" }]);

export const panelHot = style([panel, { borderColor: vars.color.violet }]);

export const band = style({
  fontFamily: vars.font.mono,
  fontSize: "11.5px",
  letterSpacing: ".12em",
  textTransform: "uppercase",
  color: vars.color.inkFaint,
});

export const flow = style({
  display: "flex",
  flexDirection: "column",
  alignItems: "flex-start",
  gap: "6px",
});

export const box = style({
  fontFamily: vars.font.mono,
  fontSize: "13px",
  padding: "7px 12px",
  borderRadius: "9px",
  border: `1px solid ${vars.color.lineStrong}`,
  background: vars.color.bg,
});

export const boxMine = style([
  box,
  {
    background: vars.color.violetTint,
    borderColor: vars.color.violet,
    color: vars.color.violetInk,
  },
]);

export const arrow = style({
  color: vars.color.inkFaint,
  paddingLeft: "12px",
});

export const linkCard = style([
  card,
  {
    display: "flex",
    flexDirection: "column",
    transition: "transform .15s ease, border-color .2s ease",
    selectors: { "&:hover": { transform: "translateY(-2px)", borderColor: vars.color.violet } },
  },
]);

export const more = style({
  marginTop: "12px",
  fontSize: "14px",
  color: vars.color.violetInk,
});
