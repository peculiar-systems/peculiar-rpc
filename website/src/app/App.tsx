import { LocationProvider, Router, Route, useLocation } from "preact-iso";
import { Landing } from "../landing/Landing.tsx";
import { DocsPage } from "../pages/DocsPage.tsx";
import { NotFound } from "../pages/NotFound.tsx";
import { Shell } from "../shell/Shell.tsx";
import "./global.css.ts";

const Docs = () => {
  const { path } = useLocation();
  return <DocsPage path={path} />;
};

export const App = () => (
  <LocationProvider>
    <Shell>
      <Router>
        <Route path="/" component={Landing} />
        <Route path="/docs" component={Docs} />
        <Route path="/docs/*" component={Docs} />
        <Route path="/404" component={NotFound} />
        <Route default component={NotFound} />
      </Router>
    </Shell>
  </LocationProvider>
);
