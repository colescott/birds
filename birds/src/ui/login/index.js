import React from "react";
import { connect } from "react-redux";
import OmniForm from "react-omniform/src";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

import { FormItem, ButtonItem } from "../components/form.js";

const Login = (props) => {
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
                    title="Login"
                />
                    <CardText>
                        <OmniForm
                            items={{
                                email: "Email",
                                password: "Password",
                            }}
                            types={{
                                password: "password"
                            }}
                            updateValue={props.handleUpdate}
                            values={props.form}
                            submitForm={props.login(props.form)}
                            FormItem={FormItem}
                            ButtonItem={ButtonItem}
                        />
                    </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = (state) => ({
    form: s.getLoginForm(state)
});

const mapDispatchToProps = (dispatch) => ({
  handleUpdate: (key, value) => dispatch(a.setLoginForm({ [ key ]: value })),
  login: (data) => () => dispatch(a.loginAuth(data))
});

export default connect(mapStateToProps, mapDispatchToProps)(Login);
