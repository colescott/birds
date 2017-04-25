import React from "react";
import ReactDOM from "react-dom";

import createHistory from "history/createBrowserHistory";

import Root from "./root.js";
import createStore from "./createStore";

const history = createHistory();
const store = createStore(history);
const rootElement = document.getElementById("root");

ReactDOM.render(<Root store={store} history={history} />, rootElement);

if (module.hot) {
    module.hot.accept("./root.js", () => {
        const NextRoot = require("./root.js").default;
        ReactDOM.render(
            <NextRoot store={store} history={history} />,
            rootElement
        );
    });
}
