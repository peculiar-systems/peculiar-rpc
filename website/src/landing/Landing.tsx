import { highlight } from "../../plugins/highlight.ts";
import { button, buttonGhost, lede, sectionTag, sectionTitle, wrap } from "../app/global.css.ts";
import * as s from "./landing.css.ts";

const sample = `import Peculiar.Rpc qualified as Rpc
import Proto.Example.V1.Echo

Rpc.deriveService ''Echo

echo :: EchoServer
echo = EchoServer{say, count}

say :: Rpc.Context -> SayRequest -> IO SayResponse
say _ request = pure (defMessage & #text .~ request ^. #text)

main :: IO ()
main = Rpc.withServer Rpc.defaultCalls [Rpc.endpoint echo] \\rpc ->
  Warp.run 8080 (Rpc.middleware rpc yourApp)`;

const numbers = [
  {
    value: "10,204",
    text: "of 10,204 server cases pass: gRPC, gRPC-Web and Connect, protobuf and JSON, over HTTP/1.1, h2c and TLS, mutual TLS and six compressions included",
  },
  {
    value: "13,038",
    text: "of 13,038 client cases pass: every protocol and both codecs over HTTP/2, the web ones over HTTP/1.1 too, with mutual TLS everywhere",
  },
  {
    value: "64 / 64",
    text: "gRPC interop runs against grpc-go: its client against this server and this client against its server, over h2c and TLS",
  },
  { value: "0", text: "proxies, sidecars or C libraries between a browser and your handler" },
] as const;

const features = [
  {
    key: "01",
    title: "Derived servers and clients",
    text: "One record per service with one field per RPC, its type fixed by the streaming kind. A missing method is a missing field.",
  },
  {
    key: "02",
    title: "Routes from the descriptor",
    text: "Paths and Connect GET permissions are read from the service descriptor. Nothing written down, so nothing drifts.",
  },
  {
    key: "03",
    title: "gRPC-Web and Connect",
    text: "gRPC-Web binary and text, Connect unary, GET and streaming, all served by the same handlers as native gRPC.",
  },
  {
    key: "04",
    title: "Limits after decompression",
    text: "Receive limits are checked against the real message size, with gzip inflation capped one byte past the limit.",
  },
  {
    key: "05",
    title: "Deadlines from arrival",
    text: "Enforced from when the request arrived, and every stream ends with a status, even one that timed out.",
  },
  {
    key: "06",
    title: "Mounts beside your routes",
    text: "The service is a WAI application and middleware: add it to warp, servant or scotty and it shares the port with your own routes.",
  },
  {
    key: "07",
    title: "A client for every protocol",
    text: "gRPC, Connect and gRPC-Web from one generated record, over HTTP/2 or HTTP/1.1, with mutual TLS, retries and interceptors.",
  },
  {
    key: "08",
    title: "Health and reflection",
    text: "grpc.health.v1, and reflection that grpcurl reads, with descriptors derived from the code.",
  },
] as const;

const guides = [
  {
    key: "01",
    title: "Getting started",
    text: "A service, a server and a client in one sitting.",
    href: "/docs/getting-started",
  },
  {
    key: "02",
    title: "Services and handlers",
    text: "What the derived records look like, and why they can't drift.",
    href: "/docs/services",
  },
  {
    key: "03",
    title: "How the protocols work",
    text: "What changes between gRPC, gRPC-Web and Connect, byte by byte.",
    href: "/docs/protocols",
  },
] as const;

export const Landing = () => (
  <>
    <div class={s.hero}>
      <div class={s.gridBg} aria-hidden="true" />
      <div class={wrap}>
        <div class={s.inner}>
          <span class={s.catTag}>
            <span class={s.catDot} aria-hidden="true" />
            Haskell library · RPC
          </span>
          <h1 class={s.title}>
            peculiar-rpc<span class={s.titleDot}>.</span>
          </h1>
          <p class={s.tagline}>gRPC, gRPC-Web and Connect. One Haskell binary. No proxy.</p>
          <p class={s.blurb}>
            Browsers can't speak gRPC, so the usual answer is an Envoy sidecar translating in front
            of your server. peculiar-rpc does that translation <b>inside the process</b>: native
            gRPC, gRPC-Web and Connect clients all reach the same handlers <b>on one port</b>,
            mounted in your own WAI server beside your own routes. The handlers are derived from
            your <code>.proto</code> files, so <b>a missing method is a missing field</b>, and a
            renamed one doesn't compile.
          </p>
          <div class={s.ctas}>
            <a class={button} href="/docs">
              Read the docs <span aria-hidden="true">→</span>
            </a>
            <a class={buttonGhost} href="/docs/conformance">
              See the conformance results
            </a>
          </div>
          <p class={s.trust}>
            Passes the official conformance suites. MIT licensed. Pure Haskell, no C libraries.
          </p>
        </div>
        <pre class={s.heroCode} tabIndex={0}>
          <span class="file">Main.hs</span>
          <code dangerouslySetInnerHTML={{ __html: highlight("haskell", sample) }} />
        </pre>
      </div>
    </div>

    <section class={s.sectionAlt} id="measured">
      <div class={wrap}>
        <p class={sectionTag}>Conformance</p>
        <h2 class={sectionTitle}>Tested against the suites the protocols' own teams use.</h2>
        <p class={lede}>
          The official Connect conformance suite and gRPC's own interop tests run against both the
          server and the client on every build, and every case passes.
        </p>
        <div class={s.cards}>
          {numbers.map((number) => (
            <div class={s.card} key={number.value}>
              <p class={s.number}>{number.value}</p>
              <p class={s.cardText}>{number.text}</p>
            </div>
          ))}
        </div>
      </div>
    </section>

    <section class={s.section} id="proxy">
      <div class={wrap}>
        <p class={sectionTag}>The usual setup</p>
        <h2 class={sectionTitle}>Take the proxy out of the picture.</h2>
        <p class={lede}>
          gRPC-Web and Connect differ from gRPC by a content type, where the trailers go, and how
          errors are written. That translation is small enough to own, and owning it removes a
          moving part from every deployment.
        </p>
        <div class={s.compare}>
          <div class={s.panel}>
            <p class={s.band}>Before</p>
            <h3 class={s.cardTitle}>A sidecar per service</h3>
            <div class={s.flow}>
              <span class={s.box}>Browser · gRPC-Web</span>
              <span class={s.arrow}>↓</span>
              <span class={s.box}>Envoy · translate</span>
              <span class={s.arrow}>↓</span>
              <span class={s.box}>Haskell · native gRPC only</span>
            </div>
            <p class={s.cardText}>
              Two processes to deploy, configure and keep in step. A reply without trailers reaches
              the browser as a phantom CORS error.
            </p>
          </div>
          <div class={s.panelHot}>
            <p class={s.band}>With peculiar-rpc</p>
            <h3 class={s.cardTitle}>One binary, every client</h3>
            <div class={s.flow}>
              <span class={s.box}>Browser · gRPC-Web or Connect</span>
              <span class={s.box}>Mobile · native gRPC</span>
              <span class={s.box}>curl · Connect unary</span>
              <span class={s.arrow}>↓</span>
              <span class={s.boxMine}>Your Haskell server · one port</span>
            </div>
            <p class={s.cardText}>
              Every reply carries a gRPC status, failures included, so a version mismatch reads as{" "}
              <code>UNIMPLEMENTED</code>, not as a network fault.
            </p>
          </div>
        </div>
      </div>
    </section>

    <section class={s.sectionAlt} id="what">
      <div class={wrap}>
        <p class={sectionTag}>What you get</p>
        <h2 class={sectionTitle}>Everything the wire needs, derived from the contract.</h2>
        <p class={lede}>
          No JSON codec, on purpose: clients are generated from the same <code>.proto</code> files.
        </p>
        <div class={s.cards}>
          {features.map((feature) => (
            <div class={s.card} key={feature.key}>
              <p class={s.cardKey}>{feature.key}</p>
              <h3 class={s.cardTitle}>{feature.title}</h3>
              <p class={s.cardText}>{feature.text}</p>
            </div>
          ))}
        </div>
      </div>
    </section>

    <section class={s.section} id="native">
      <div class={wrap}>
        <p class={sectionTag}>All of it in one package</p>
        <h2 class={sectionTitle}>Every protocol is served by the same code.</h2>
        <p class={lede}>
          Native gRPC, the gRPC-Web and Connect translation, the HTTP/2 client and the unary fast
          path are all peculiar-rpc, on the http2 package. Owning the whole path made unary calls
          faster than connect-go and a 1000-message stream eight times faster than it was on a
          general-purpose gRPC library.
        </p>
        <div class={s.cards}>
          {guides.map((guide) => (
            <a class={s.linkCard} href={guide.href} key={guide.key}>
              <p class={s.cardKey}>{guide.key}</p>
              <h3 class={s.cardTitle}>{guide.title}</h3>
              <p class={s.cardText}>{guide.text}</p>
              <span class={s.more}>Read →</span>
            </a>
          ))}
        </div>
      </div>
    </section>
  </>
);
