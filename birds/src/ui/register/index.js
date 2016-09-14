import React from "react"; 
import { connect } from 'react-redux';

import * as a from '../../store/actions.js';
import * as s from '../../store/selectors.js';

const Register = (props) => {
  return (
    <div>
      <h2> Register </h2>
      <input
        type='text'
        placeholder='Email'
        value={props.email || ''}
        onChange={props.updateKey('email')}
      />
      <br />
      <input
        type="text"
        placeholder='Password'
        value={props.password || ''}
        onChange={props.updateKey('password')}
      />
      <br />
      <input
        type="text"
        placeholder='First Name'
        value={props.firstname || ''}
        onChange={props.updateKey('firstname')}
      />
      <br />
      <input
        type="text"
        placeholder="Last Name"
        value={props.lastname || ''}
        onChange={props.updateKey('lastname')}
      />
      <br />
      <input
        type="text"
        placeholder="Team Number"
        value={props.teamnumber || ''}
        onChange={props.updateKey('teamnumber')}
      />
      <br />
      <button onClick={props.addUser(props.email, props.password)}> Register </button>
    </div>
  )
}

const mapStateToProps = (state) => ({
    ...s.getRegisterForm(state)
});

const mapDispatchToProps = (dispatch) => ({
  updateKey: (key) => (e) => dispatch(a.setRegisterForm({[key]: e.target.value})),
  addUser: (email, password) => () => dispatch(a.addUser(email, password))
});

export default connect(mapStateToProps, mapDispatchToProps)(Register);
