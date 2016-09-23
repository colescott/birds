import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import trim from "trim";

import api from "../../api";

import * as c from "../constants.js";
import * as s from "../selectors.js";
import * as a from "../actions.js";

function* auth() {
    for (;;) {
        try {
            // Wait for auth realated actions
            const action = yield take([
                c.AUTH_LOGOUT,
                c.AUTH_LOGIN,
                c.AUTH_REGISTER
            ]);
            switch (action.type) {
                case c.AUTH_LOGIN: {
                    // Gather relevent params
                    const { email, password } = action.payload;

                    // Login
                    const { user, token } = yield call(authWrapper, login, email, password);
                    yield put(a.setUser({
                        token,
                        ...user
                    }));
                    yield put(a.resetLoginForm());

                    // Redirect to the correct page
                    if (!user.teamnumber)
                        yield put(push("/selectTeam"));
                    else
                        yield put(push("/"));
                    break;
                }
                case c.AUTH_LOGOUT: {
                    // Logout
                    yield call(logout);

                    // Redirect home
                    yield put(push("/"));
                    break;
                }
                case c.AUTH_REGISTER: {
                    // Gether relevent params
                    const user = yield select(s.getRegisterForm);

                    // Register
                    yield call(authWrapper, register, user);
                    yield put(a.resetRegisterForm());

                    // Login
                    const { user: userData, token } = yield call(authWrapper, login, user.email, user.password);
                    yield put(a.setUser({
                        token,
                        ...userData
                    }));

                    // Redirect to the select team page
                    yield put(push("/selectTeam"));
                    break;
                }
                default:
                    break;
            }
        } catch (e) {
            // If there is an error, dispatch an error action
            yield put(a.authFailure(e.message));
        }
    }
}

export function* login(email, password) {
    const res = yield call(api.auth.login, trim(email), password);
    if (res.error) throw new Error(res.error.message);
    return res.data;
}

export function* logout() {
    yield put(a.resetUser());
}

export function* register(user) {
    const res = yield call(api.auth.register, {
        ...user,
        email: trim(user.email),
        firstname: trim(user.firstname),
        lastname: trim(user.lastname)
    });
    if (res.error) throw new Error(res.error.message);
    return res.data.user;
}

export function* getUser(id, token) {
    const res = yield call(api.auth.getUser, id, token);
    if (res.error) throw new Error(res.error.message);
    return res.data.user;
}

export function* authWrapper(func, ...args) {
    yield put(a.authLoad());
    const res = yield call(func, ...args);
    yield put(a.authSuccess());
    return res;
}

export default auth;
