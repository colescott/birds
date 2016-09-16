import React from "react";
import { connect } from "react-redux";

import * as s from "../../store/selectors.js";
import * as a from "../../store/actions.js";

import { Card, CardHeader, CardText } from "material-ui/Card";
import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";

const RegisterSuccess = (props) => {
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
                    title="Register Success"
                />
                <CardText>
                    <h2> Make a Team </h2>
                    <TextField
                        hintText="Team Name"
                        onChange={props.updateKey("nName")}
                        value={props.form.nName || ""}
                    /><br />
                    <TextField
                        hintText="Team Number"
                        onChange={props.updateKey("nNumber")}
                        value={props.form.nNumber || ""}
                    /><br />
                    <FlatButton label="Create" onClick={props.createTeam(props.form.nName, props.form.nNumber)} />
                    <h2> Join a Team </h2>
                    <TextField
                        hintText="Team Name"
                        onChange={props.updateKey("name")}
                        value={props.form.name || ""}
                    /><br />
                    <TextField
                        hintText="Team Password"
                        onChange={props.updateKey("pass")}
                        value={props.form.pass || ""}
                    /><br />
                    <FlatButton label="Join"/>
                </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = (state) => ({
    form: s.getTeamForm(state)
});

const mapDispatchToProps = (dispatch) => ({
    updateKey: (key) => (e, value) => dispatch(a.setTeamForm({ [ key ]: value })),
    createTeam: (name, number) => () => dispatch(a.createTeam(name, number)),
    joinTeam: (number, pass) => () => dispatch(a.joinTeam(number, pass))
});

export default connect(mapStateToProps, mapDispatchToProps)(RegisterSuccess);
