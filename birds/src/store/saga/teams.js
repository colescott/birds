import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

function* teams() {
    while (true) {
        const action = yield take([
            c.JOIN_TEAM,
            c.CREATE_TEAM
        ]);
    }
}

export default teams;
