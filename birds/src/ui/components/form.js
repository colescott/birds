import React from "react";

import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";

export const FormItem = ({ display, handleChange, value, error, name }) =>
    <TextField
      errorText={error}
      floatingLabelText={display}
      value={value}
      onChange={(e, value) => handleChange(value)}
      name={name}
    />;

export const ButtonItem = () => <FlatButton label={"Submit"} type="submit"/>;
