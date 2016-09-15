import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardActions, CardHeader, CardText } from "material-ui/Card";
import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";

const Register = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            <Card>
                <CardHeader
                    title="Register"
                />
                <CardText>
                    <TextField
                        floatingLabelText="Email"
                        onChange={props.updateKey("email")}
                        value={props.form.email || ""}
                    /><br />
                    <TextField
                        floatingLabelText="Password"
                        type="password"
                        onChange={props.updateKey("password")}
                        value={props.form.password || ""}
                    /><br />
                    <TextField
                        floatingLabelText="First Name"
                        onChange={props.updateKey("firstname")}
                        value={props.form.firstname || ""}
                    /><br />
                    <TextField
                        floatingLabelText="Last Name"
                        onChange={props.updateKey("lastname")}
                        value={props.form.lastname || ""}
                    /><br />
                    <TextField
                        floatingLabelText="Team Number"
                        onChange={props.updateKey("teamnumber")}
                        type="number"
                        value={props.form.teamnumber || ""}
                    /><br />
                </CardText>
                <CardActions
                    style={{
                        display: "flux",
                        alignItems: "space-around"
                    }}
                >
                     <FlatButton label="Register" onClick={props.register()}/>
                </CardActions>
            </Card>
        </div>
    );
};

const mapStateToProps = (state) => ({
    form: s.getRegisterForm(state)
});

const mapDispatchToProps = (dispatch) => ({
  updateKey: (key) => (e, value) => dispatch(a.setRegisterForm({ [ key ]: value })),
  register: () => () => dispatch(a.registerAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Register);
