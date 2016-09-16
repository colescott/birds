import React from "react";
import { connect } from "react-redux";

import { Card, CardHeader, CardText } from "material-ui/Card";
import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";

const RegisterSuccess = () => {
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
                    /><br />
                    <TextField
                        hintText="Team Number"
                    /><br />
                    <FlatButton label="Create" />
                    <h2> Join a Team </h2>
                    <TextField
                        hintText="Team Name"
                    /><br />
                    <TextField
                        hintText="Team Password"
                    /><br />
                    <FlatButton label="Join" />
                </CardText>
            </Card>
        </div>
    );
};

const mapStateToProps = () => ({});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(RegisterSuccess);
