import React, { Component } from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as c from "../../store/constants.js"
import * as s from "../../store/selectors.js";

const Register = (props) => {
  return (
    <div>
      <h2> Register </h2>
      <input
        type="text"
        placeholder="email"
        value={props.email}
        onChange={props.updateKey("email")}
      />
      <br />
      <input
        type="password"
        placeholder="password"
        value={props.password}
        onChange={props.updateKey("password")}
      />
      <br />
      <button onClick={props.addUser(props.email, props.password)}> Register </button>
    </div>
  )
}

const mapStateToProps = (state) => ({
  email: (console.log(s.getRegisterForm(state)), s.getRegisterForm(state).email || ""),
  password: s.getRegisterForm(state).password || ""
});

const mapDispatchToProps = (dispatch) => ({
  updateKey: (key) => (e) => dispatch(a.setRegisterForm(key, e.target.value)),
  addUser: (email, password) => () => dispatch(a.addUser(email, password))
});

export default connect(mapStateToProps, mapDispatchToProps)(Register);
