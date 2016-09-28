import React, { PropTypes } from "react";
import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";

import { checker } from "../../util.js";

const basicValidation = (text) => {
    return text == ""
    ? "You must fill in this field."
    : null;
};

const Form = (props) => {
    const errors =
    props.items.map(item =>
        item === null
        ? null
        : checker(
            props.values,
            basicValidation,
            props.validation[ item ] || (() => null),
        )(props.values[ item ])
    );
    const valid = !errors.reduce((a, b) => a || b);
    return (
        <form onSubmit={e => {
            e.preventDefault();
            props.handleSubmit();
        }}
        >
            {
                props.items
                    .map((item, i) =>
                        <FormField
                            handleUpdate={props.handleUpdate}
                            type={props.types[ item ]}
                            label={props.labels[ item ]}
                            item={item}
                            value={props.values[ item ]}
                            error={errors[ i ]}
                        />
                    )
                    .map((item, i) =>
                        <div key={i}>
                            { item }
                        </div>
                    )
            }
            <Submit handleSubmit={props.handleSubmit} disabled={!valid} label={props.submitLabel}/>
        </form>
    );
};

Form.propTypes = {
    handleUpdate: PropTypes.func,
    handleSubmit: PropTypes.func.isRequired,
    types: PropTypes.object.isRequired,
    labels: PropTypes.object.isRequired,
    submitLabel: PropTypes.string,
    values: PropTypes.object.isRequired,
    items: PropTypes.array.isRequired,
    validation: PropTypes.object
};

Form.defaultProps = {
    validation: {}
};

const Submit = ({ handleSubmit, label = "Submit", disabled }) =>
    <FlatButton disabled={disabled} type="submit" label={label} onClick={handleSubmit}/>;

const FormField = ({ handleUpdate, type, label, item, value, error }) =>
    <TextField
        floatingLabelText={label}
        type={type}
        onChange={(_, v) => handleUpdate({
            [ item ]: v
        })}
        value={value || ""}
        errorText={error}
    />;

export default Form;
