import { takeEvery } from "redux-saga";
import { take, fork, put } from "redux-saga/effects";

import api from "../../api/index.js";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* addUser(action) {
  const { payload } = action;
  const { email, password, firstname, lastname, teamnumber } = payload;
  try {
    const res = yield api.v1.addUser(payload);
    alert("You have been registired.");
    yield put(a.setRegisterForm("email", ""));
    yield put(a.setRegisterForm("password", ""));
    yield put(a.setRegisterForm("firstname", ""));
    yield put(a.setRegisterForm("lastname", ""));
    yield put(a.setRegisterForm("teamnumber", ""));
  }
  catch (e) {
    alert(e);
  }
}


function* rootSaga() {
  yield* takeEvery(c.ADD_USER, addUser);
}

export default rootSaga;
