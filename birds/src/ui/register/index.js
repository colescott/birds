import React from "react";
import { connect } from "react-redux";
import OmniForm from "react-omniform/src";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

import { FormItem, ButtonItem } from "../components/form.js";
import { isValidEmail } from "../../util";

const passWordsMustMatch = (text, form) => {
    if (form.password2 != form.password) throw Error("Passwords must match");
};

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
                    <OmniForm
                        items={{
                            email: "Email",
                            password: "Password",
                            password2: "Password Again",
                            firstname: "First Name",
                            lastname: "Last Name"
                        }}
                        updateValue={props.handleUpdate}
                        values={props.form}
                        submitForm={props.register}
                        customValidation={{
                            email: isValidEmail,
                            password2: passWordsMustMatch,
                        }}
                        FormItem={FormItem}
                        ButtonItem={ButtonItem}
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
  handleUpdate: (key, value) => dispatch(a.setRegisterForm({ [ key ]: value })),
  register: () => dispatch(a.registerAuth())
});

export default connect(mapStateToProps, mapDispatchToProps)(Register);
