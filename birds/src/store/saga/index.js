import { takeEvery } from "redux-saga";
import { take, fork } from "redux-saga/effects";

import api from "../../api/index.js";

import * as c from "../constants.js";

function* addUser(firebase, action) {
  const { payload } = action;
  const { email, password } = payload;
  try {
    const res = api.firebase.addUser(firebase, {email, password});
  }
  catch (e) {
    console.error(e);
  }
}


function* rootSaga() {
  const { payload: firebase } = yield take(c.FIREBASE_SET);
  yield* takeEvery(c.ADD_USER, addUser, firebase);
}

export default rootSaga;
