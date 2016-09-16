import { call, put } from "redux-saga/effects";

import api from "../../api";

import * as a from "../actions.js";

export function* login(email, password) {
    const res = yield call(api.auth.login, email, password);
    if (res.error) throw new Error(res.error.message);
    yield put(a.setAuth({
        token: res.data.token,
        ...res.data.user
    }));
}

export function* logout() {
    yield put(a.resetAuth());
}

export function* register(user) {
    const res = yield call(api.auth.register, user);
    if (res.error) throw new Error(res.error.message);
    yield put(a.setAuth(res.data.user));
}
