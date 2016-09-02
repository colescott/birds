import { takeEvery } from "redux-saga";
import { take, fork, put } from "redux-saga/effects";

import api from "../../api/index.js";

import * as c from "../constants.js";
import * as a from "../actions.js";

function* addUser(firebase, action) {
  const { payload } = action;
  const { email, password } = payload;
  try {
    const res = yield api.firebase.addUser(firebase, {email, password});
    alert("You have been registired.");
    yield put(a.setRegisterForm("email", ""));
    yield put(a.setRegisterForm("password", ""));
  }
  catch (e) {
    alert(e);
  }
}


function* rootSaga() {
  const { payload: firebase } = yield take(c.FIREBASE_SET);
  yield* takeEvery(c.ADD_USER, addUser, firebase);
}

export default rootSaga;
