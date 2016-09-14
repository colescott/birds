import { take, call, put } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* auth() {
    while (true) {
        const action = yield take([
            c.LOGOUT_AUTH,
            c.LOGIN_AUTH,
            c.REGISTER_AUTH
        ]);
        switch (action.type) {
            case c.LOGIN_AUTH: {
                const { username, password } = action.payload;
                const res = yield call(api.auth.login, username, password);
                if (res.error) {
                    yield put(a.setAuth(new Error(res.error.message)));
                    break;
                }
                yield put(a.setAuth(res.data));
                break;
            }
            case c.LOGOUT_AUTH: {
                yield put(a.resetAuth());
            }
        }
    }
}

export default auth;
