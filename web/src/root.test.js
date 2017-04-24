import React from "react";
import ReactDOM from "react-dom";
import createHistory from "history/createBrowserHistory";
import { createStore } from "redux";

import Root from "./root";

const history = createHistory();
const store = createStore(() => ({}));

it("Should render without errors", () => {
    const div = document.createElement("div");
    ReactDOM.render(<Root store={store} history={history} />, div);
});
