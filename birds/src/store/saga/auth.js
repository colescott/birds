import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

import { login, logout, register } from "./user.js";

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
                    const { email, password } = action.payload;
                    const user = yield call(login, email, password);
                    if (!user.teamnumber)
                        yield put(push("/selectTeam"));
                    else
                        yield put(push("/"));
                    break;
                }
                case c.LOGOUT_AUTH: {
                    yield call(logout);
                    yield put(push("/"));
                    break;
                }
                case c.REGISTER_AUTH: {
                    const user = yield select(s.getRegisterForm);
                    yield call(register, user);
                    yield call(login, user.email, user.password);
                    yield put(push("/selectTeam"));
                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            yield put(a.setAuth(e));
        }
    }
}

export default auth;
