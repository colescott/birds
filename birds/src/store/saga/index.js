import { takeEvery, fork } from "redux-saga";
import { put, call } from "redux-saga/effects";

import api from "../../api/index.js";

import * as c from "../constants.js";
import * as a from "../actions.js";

import auth from "./auth";

function* addUser(action) {
    const { payload } = action;
    yield fork(auth);
    try {
        const res = yield call(api.v1.addUser, payload);
        if (res.error) {
            throw new Error(res.error.message);
        }
        yield put(a.resetRegisterForm());
    } catch (e) {
      console.error(e);
    }
}


function* rootSaga() {
  yield* takeEvery(c.ADD_USER, addUser);
}

export default rootSaga;
