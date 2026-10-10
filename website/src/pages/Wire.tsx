import { useEffect, useRef } from "preact/hooks";
import { visuallyHidden } from "../app/global.css.ts";
import * as s from "./pages.css.ts";

/*
 * Three requests ride three protocols into one port, the handler looks for the page,
 * shrugs, and each answer goes back in its own protocol's shape.
 */

type Cell = { ch: string; cls: string };
type Grid = Cell[][];
type Point = readonly [number, number];

const W = 60;
const H = 9;
const J = 28;
const PORT = J + 1;
const LABEL = PORT + 2;
const BOX = 44;
const TICK = 55;
const STAGGER = 5;

const lanes = [
  { name: "gRPC", row: 0, cls: "v", answer: "<- HTTP 200 · grpc-status: 5 (NOT_FOUND)" },
  { name: "gRPC-Web", row: 2, cls: "l", answer: "<- HTTP 200 · grpc-status: 5 (NOT_FOUND)" },
  { name: "Connect", row: 4, cls: "m", answer: '<- HTTP 404 · {"code": "not_found"}' },
] as const;

const range = (from: number, to: number): number[] =>
  Array.from({ length: to - from + 1 }, (_, i) => from + i);

const trunk: Point[] = range(PORT, BOX - 1).map((x): Point => [x, 2]);
const paths: Point[][] = [
  [...range(11, J - 1).map((x): Point => [x, 0]), [J, 1], [J, 2], ...trunk],
  [...range(11, J).map((x): Point => [x, 2]), ...trunk],
  [...range(11, J - 1).map((x): Point => [x, 4]), [J, 3], [J, 2], ...trunk],
];

const lengthOf = (k: number): number => paths[k]?.length ?? 0;
const arrivals = paths.map((path, k) => k * STAGGER + path.length);
const SEARCH = Math.max(...arrivals) + 3;
const HMM = SEARCH + 32;
const SHRUG = HMM + 10;
const BACK = SHRUG + 8;
const typed = lanes.map((_, k) => BACK + lengthOf(k) + k * 6);
const LOOP = Math.max(...lanes.map((lane, k) => (typed[k] ?? 0) + lane.answer.length / 2)) + 70;

const put = (grid: Grid, x: number, y: number, text: string, cls = ""): void => {
  for (let i = 0; i < text.length; i += 1) {
    const cell = grid[y]?.[x + i];
    if (cell !== undefined) {
      cell.ch = text.charAt(i);
      cell.cls = cls;
    }
  }
};

// Inside the port label a packet lights the label up instead of covering it.
const inPort = ([x, y]: Point): boolean => y === 2 && x >= LABEL && x < LABEL + 8;

const paint = (grid: Grid, point: Point, ch: string, cls: string): void => {
  if (inPort(point)) {
    put(grid, LABEL, 2, "[ :443 ]", cls);
    return;
  }
  put(grid, point[0], point[1], ch, cls);
};

const centred = (grid: Grid, y: number, text: string, cls = ""): void => {
  put(grid, BOX + 1 + Math.floor((13 - text.length) / 2), y, text, cls);
};

const mood = (f: number): readonly [string, string] => {
  if (f >= SHRUG) {
    return [String.raw`¯\_(-_-)_/¯`, "not found"];
  }
  if (f >= HMM) {
    return ["(-_-)", "hmm"];
  }
  if (f >= SEARCH) {
    const look = Math.floor(f / 4) % 2 === 0 ? "(<_<)" : "(>_>)";
    return [look, `searching${".".repeat(Math.floor(f / 4) % 4)}`];
  }
  if (arrivals.some((a) => f >= a && f < a + 4)) {
    return ["(O_O)", "a request!"];
  }
  return ["(o_o)", "handler"];
};

const scenery = (grid: Grid): void => {
  for (const lane of lanes) {
    put(grid, 1, lane.row, lane.name, "b");
  }
  put(grid, 11, 0, `${"-".repeat(J - 12)}.`, "d");
  put(grid, J, 1, "\\", "d");
  put(grid, 11, 2, `${"-".repeat(J - 11)}+`, "d");
  put(grid, J, 3, "/", "d");
  put(grid, 11, 4, `${"-".repeat(J - 12)}'`, "d");
  put(grid, PORT, 2, "--", "d");
  put(grid, LABEL, 2, "[ :443 ]", "b");
  put(grid, LABEL + 8, 2, "---->", "d");
  put(grid, BOX, 0, ".-------------.", "d");
  for (const y of [1, 2, 3]) {
    put(grid, BOX, y, "|", "d");
    put(grid, BOX + 14, y, "|", "d");
  }
  put(grid, BOX, 4, "'-------------'", "d");
};

const traffic = (grid: Grid, f: number, k: number): void => {
  const path = paths[k] ?? [];
  const cls = lanes[k]?.cls ?? "";
  const out = f - k * STAGGER;
  for (const tail of [3, 2, 1]) {
    const point = path[out - tail];
    if (point !== undefined && out < path.length + tail) {
      paint(grid, point, "=", cls);
    }
  }
  const head = path[out];
  if (head !== undefined) {
    paint(grid, head, ">", cls);
  }
  const back = path.length - 1 - (f - BACK);
  const behind = path[back + 1];
  if (f >= BACK && behind !== undefined) {
    paint(grid, behind, "=", cls);
  }
  const ret = path[back];
  if (f >= BACK && ret !== undefined) {
    paint(grid, ret, "<", cls);
  }
};

const frame = (f: number): Grid => {
  const grid: Grid = Array.from({ length: H }, () =>
    Array.from({ length: W }, (): Cell => ({ ch: " ", cls: "" })),
  );
  scenery(grid);
  const [face, label] = mood(f);
  centred(grid, 2, face, f >= SHRUG ? "m" : "b");
  centred(grid, 3, label, "d");
  for (const [k, lane] of lanes.entries()) {
    traffic(grid, f, k);
    const shown = Math.max(0, (f - (typed[k] ?? 0)) * 2);
    put(grid, 1, 6 + k, lane.name, shown > 0 ? "b" : "d");
    put(grid, 11, 6 + k, lane.answer.slice(0, shown), lane.cls);
  }
  return grid;
};

const escape = (ch: string): string =>
  ch.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");

const html = (grid: Grid): string =>
  grid
    .map((row) => {
      let out = "";
      let open = "";
      for (const cell of row) {
        if (cell.cls !== open) {
          out += open === "" ? "" : "</span>";
          out += cell.cls === "" ? "" : `<span class="${cell.cls}">`;
          open = cell.cls;
        }
        out += escape(cell.ch);
      }
      return open === "" ? out : `${out}</span>`;
    })
    .join("\n");

export const Wire = () => {
  const ref = useRef<HTMLPreElement>(null);
  useEffect(() => {
    if (globalThis.matchMedia("(prefers-reduced-motion: reduce)").matches) {
      return undefined;
    }
    let f = 0;
    const timer = setInterval(() => {
      if (ref.current !== null) {
        ref.current.innerHTML = html(frame(f));
      }
      f = (f + 1) % LOOP;
    }, TICK);
    return () => {
      clearInterval(timer);
    };
  }, []);
  return (
    <>
      <pre
        ref={ref}
        class={s.wire}
        style={{ fontSize: `min(13.5px, calc((100vw - 92px) / ${W * 0.6}))` }}
        aria-hidden="true"
        dangerouslySetInnerHTML={{ __html: html(frame(LOOP - 1)) }}
      />
      <p class={visuallyHidden}>
        gRPC and gRPC-Web answer HTTP 200 with grpc-status 5, not found; Connect answers HTTP 404
        with the code not_found. Same handler, one port.
      </p>
    </>
  );
};
