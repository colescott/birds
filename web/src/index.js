import React from "react";
import ReactDOM from "react-dom";

import createHistory from "history/createBrowserHistory";

import Root from "./root";
import createStore from "./createStore";

const history = createHistory();
const store = createStore(history);
const rootElement = document.getElementById("root");

ReactDOM.render(<Root store={store} history={history} />, rootElement);

if (module.hot) {
    module.hot.accept("./root", () => {
        const NextRoot = require("./root");
        ReactDOM.render(<NextRoot />, rootElement);
    });
}
