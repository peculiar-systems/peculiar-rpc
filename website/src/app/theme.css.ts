import { createGlobalTheme } from "@vanilla-extract/css";

export const vars = createGlobalTheme(":root", {
  color: {
    bg: "oklch(0.991 0.006 95)",
    bg2: "oklch(0.975 0.008 95)",
    surface: "oklch(1 0 0)",
    ink: "oklch(0.21 0.012 270)",
    inkSoft: "oklch(0.43 0.012 270)",
    inkFaint: "oklch(0.55 0.012 270)",
    line: "oklch(0.9 0.008 95)",
    lineStrong: "oklch(0.83 0.01 95)",
    magenta: "#DB1F7A",
    magentaStrong: "#B4145F",
    lime: "oklch(0.86 0.21 124)",
    limeStrong: "oklch(0.77 0.2 124)",
    violet: "oklch(0.55 0.26 296)",
    violetInk: "oklch(0.42 0.22 296)",
    violetHover: "oklch(0.36 0.2 296)",
    violetTint: "oklch(0.55 0.26 296 / 0.09)",
    violetTint2: "oklch(0.55 0.26 296 / 0.18)",
    codeBg: "oklch(0.21 0.012 270)",
    codeInk: "#f3f3ef",
  },
  font: {
    display: '"Bricolage Grotesque", sans-serif',
    body: '"Geist", system-ui, sans-serif',
    mono: '"Geist Mono", ui-monospace, SFMono-Regular, Menlo, monospace',
  },
  radius: "16px",
  shadowSm: "0 1px 2px oklch(0.2 0.02 270 / 0.05), 0 1px 1px oklch(0.2 0.02 270 / 0.04)",
  maxw: "1080px",
});
