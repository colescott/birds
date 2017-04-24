// @flow

import React, { Component } from "react";

export default class Alert extends Component {
    props: {
        text?: ?string,
        type: string
    };
    render() {
        if (!this.props.text) return null;
        return (
            <div className={`alert alert-${this.props.type}`}>
                {this.props.text}
            </div>
        );
    }
}
