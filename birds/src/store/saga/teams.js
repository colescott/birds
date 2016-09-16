import { take, call, put, select } from "redux-saga/effects";
import { push } from "react-router-redux";

import api from "../../api";

import * as c from "../constants.js";
import * as a from "../actions.js";
import * as s from "../selectors.js";

function* teams() {
    console.log("loaded");
    while (true) {
        try {
            const action = yield take([
                c.JOIN_TEAM,
                c.CREATE_TEAM
            ]);
            console.log("taken");
            const { token } = yield select(s.getAuth);
            switch (action.type) {
                case c.JOIN_TEAM: {
                    const { number, pass } = action.payload;
                    console.log(action.payload);
                    const { id: uid } = yield select(s.getAuth);
                    yield call(api.teams.join, number, pass, uid, token);
                    const res = yield call(api.auth.getUser, uid, token);
                    yield put(a.setAuth(res));
                    yield put(push("/"));
                    break;
                }
                case c.CREATE_TEAM: {
                    const { name, number } = action.payload;
                    console.log(action.payload);
                    yield call(api.teams.create, name, number, token);
                    const { id: uid } = yield select(s.getAuth);
                    const res = yield call(api.auth.getUser, uid, token);
                    yield put(a.setAuth(res));
                    yield put(push("/"));
                    break;
                }
                default:
                    break;
            }
        }
        catch (e) {
            console.error(e);
        }
    }
}

export default teams;
