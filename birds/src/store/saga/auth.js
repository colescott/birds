import { take, call, put, select } from "redux-saga/effects";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

function* auth() {
    for (;;) {
        try {
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
                    break;
                }
                case c.REGISTER_AUTH: {
                    const user = yield select(s.getRegisterForm);
                    const res = yield call(api.auth.register, user);
                    if (res.error) throw new Error(res.error.message);
                    yield put(a.setAuth(res.data.user));
                    yield put(a.loginAuth());
                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            console.error(e);
            yield put(a.setAuth(e));
        }
    }
}

export default auth;
