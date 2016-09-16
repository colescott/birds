import { combineReducers } from "redux";

import kvs from "./kvs.js";

export default combineReducers({
  registerForm: kvs("REGISTER_FORM"),
  auth: kvs("AUTH"),
  loginForm: kvs("LOGIN_FORM")
});
