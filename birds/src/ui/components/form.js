import React, { PropTypes } from "react";
import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";

const Form = (props) =>
    <form onSubmit={e => {
        e.preventDefault();
        props.handleSubmit();
    }}
    >
        {
            props.items
                .map(item =>
                    <FormField
                        handleUpdate={props.handleUpdate}
                        type={props.types[ item ]}
                        label={props.labels[ item ]}
                        item={item}
                        value={props.values[ item ]}
                    />
                )
                .map((item, i) =>
                    <div key={i}>
                        { item }
                    </div>
                )
        }
        <Submit handleSubmit={props.handleSubmit} label={props.submitLabel}/>
    </form>;

Form.propTypes = {
    handleUpdate: PropTypes.func,
    handleSubmit: PropTypes.func.isRequired,
    types: PropTypes.object.isRequired,
    labels: PropTypes.object.isRequired,
    submitLabel: PropTypes.string,
    values: PropTypes.object.isRequired,
    items: PropTypes.array.isRequired
};

const Submit = ({ handleSubmit, label = "Submit" }) =>
    <FlatButton type="submit" label={label} onClick={handleSubmit}/>;

const FormField = ({ handleUpdate, type, label, item, value }) =>
    <TextField
        floatingLabelText={label}
        type={type}
        onChange={(_, v) => handleUpdate({
            [ item ]: v
        })}
        value={value || ""}
    />;

export default Form;
