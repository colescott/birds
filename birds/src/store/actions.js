import { createAction } from "redux-actions";

import * as c from "./constants.js";

export const addUser = createAction(c.ADD_USER, (email, password) => ({
  email,
  password
}));

export const setFirebase = createAction(c.FIREBASE_SET);
