// @flow

import React, { Component } from "react";

export default class TextInput extends Component {
    props: {
        placeHolder: string,
        className: string,
        value: string,
        type: string,
        onChange: (value: string) => void,
        error?: ?string,
        label: string,
        onBlur: () => void
    };
    static defaultProps = {
        placeHolder: "",
        value: "",
        type: "text",
        onChange: (_: string) => undefined,
        label: "",
        onBlur: () => undefined,
        className: ""
    };
    render() {
        const {
            placeHolder,
            value,
            className,
            type,
            error,
            label
        } = this.props;
        const { onBlur } = this.props;
        const onChange = e => this.props.onChange(e.target.value);

        const divClass = error ? "has-danger" : "";
        return (
            <div className={`form-group ${divClass}`}>
                {label
                    ? <label className="form-control-label" htmlFor="input">
                          {" "}{label}{" "}
                      </label>
                    : null}
                <input
                    id="input"
                    className={`${className} form-control`}
                    placeholder={placeHolder}
                    value={value}
                    onChange={onChange}
                    type={type}
                    onBlur={onBlur}
                />
                {error
                    ? <div className="form-control-feedback"> {error} </div>
                    : null}
            </div>
        );
    }
}
