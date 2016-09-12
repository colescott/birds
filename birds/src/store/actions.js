import { createAction } from "redux-actions";

import * as c from "./constants.js";

export const addUser = createAction(c.ADD_USER, (email, password, firstname, lastname, teamnumber) => ({
  email,
  password,
  firstname,
  lastname,
  teamnumber
}));

export const setRegisterForm = createAction(c.REGISTER_FORM_SET, (key, value) => ({
  key,
  value
}));
