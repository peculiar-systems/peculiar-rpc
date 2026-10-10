import { button, wrap } from "../app/global.css.ts";
import * as s from "./pages.css.ts";
import { Wire } from "./Wire.tsx";

export const NotFound = () => (
  <div class={`${wrap} ${s.notFound}`}>
    <p class={s.kicker}>404</p>
    <h1>Nothing here.</h1>
    <p style={{ marginTop: "12px", marginBottom: "24px" }}>
      The page does not exist, or it moved. Here is the same answer in all three protocols.
    </p>
    <Wire />
    <a class={button} href="/">
      Back to peculiar-rpc
    </a>
  </div>
);
