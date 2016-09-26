import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

import Form from "../components/form";

const isValidEmail = (text = "") =>
    text.includes("@")
    ? null
    : "Email must include @";

const passWordsMustMatch = (text, form) =>
    form.password == form.password2 && form.password != null
    ? null
    : "Passwords must match"

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
                    <Form
                        handleUpdate={props.handleUpdate()}
                        handleSubmit={props.register()}
                        items={[
                            "email",
                            "firstname",
                            "lastname",
                            "password",
                            "password2"
                        ]}
                        labels={{
                            email: "Email ",
                            password: "Password",
                            password2: "Password Again",
                            firstname: "First Name",
                            lastname: "Last Name",
                        }}
                        types={{
                            password: "password",
                            password2: "password"
                        }}
                        validation={{
                            email: isValidEmail,
                            password2: passWordsMustMatch
                        }}
                        values={props.form}
                    />
                </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = (state) => ({
    form: s.getRegisterForm(state)
});

const mapDispatchToProps = (dispatch) => ({
  handleUpdate: () => (obj) => dispatch(a.setRegisterForm({ ...obj })),
  register: () => () => dispatch(a.registerAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Register);
