import { takeEvery } from "redux-saga";
import { put, call } from "redux-saga/effects";

import api from "../../api/index.js";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* addUser(action) {
    const { payload } = action;
    try {
        const res = yield call(api.v1.addUser, payload);
        if (res.error) {
            throw new Error(error.message);
        }
        console.log("Success!");
        yield put(a.resetRegisterForm());
    }
    catch (e) {
      console.error(e);
    }
}


function* rootSaga() {
  yield* takeEvery(c.ADD_USER, addUser);
}

export default rootSaga;
